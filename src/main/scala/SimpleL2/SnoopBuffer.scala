package SimpleL2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import Utils.GenerateVerilog
import SimpleL2.Configs._
import SimpleL2.Bundles._
import freechips.rocketchip.util.SeqToAugmentedSeq
import xs.utils.{FastArbiter, ParallelMax}
import Utils.LeakChecker

object SnpBufState {
    val width   = 2
    val INVALID = "b00".U(width.W) // 0
    val WAIT    = "b01".U(width.W) // 1
    val SENT    = "b10".U(width.W) // 2
}

class SnpBufEntry(implicit p: Parameters) extends L2Bundle {
    val age   = UInt(log2Ceil(nrSnpBufEntry).W)
    val state = UInt(SnpBufState.width.W)
    val task  = new TaskBundle
    val ready = Bool()
}

class SnpBufReplay(implicit p: Parameters) extends L2Bundle {
    val shouldReplay = Bool()
    val txnID        = UInt(chiBundleParams.txnIdBits.W)
}

class SnoopBuffer()(implicit p: Parameters) extends L2Module {
    val io = IO(new Bundle {
        val taskIn     = Flipped(DecoupledIO(new TaskBundle))
        val taskOut    = DecoupledIO(new TaskBundle)
        val replay_s4  = Flipped(ValidIO(new SnpBufReplay))
        val mshrStatus = Vec(nrMSHR, Input(new MshrStatus))
    })

    val issueArb = Module(new FastArbiter(new TaskBundle, nrSnpBufEntry))
    val buffers  = RegInit(VecInit(Seq.fill(nrSnpBufEntry)(0.U.asTypeOf(new SnpBufEntry))))
    val freeVec  = VecInit(buffers.map(_.state === SnpBufState.INVALID)).asUInt
    val hasEntry = freeVec.orR
    val insertOH = PriorityEncoderOH(freeVec)

    val taskOut = WireInit(0.U.asTypeOf(Decoupled(new TaskBundle)))
    io.taskIn.ready := hasEntry

    val replay_s4      = io.replay_s4.bits
    val replayMatchVec = VecInit(buffers.map { buf => buf.task.txnID === replay_s4.txnID && buf.state =/= SnpBufState.INVALID }).asUInt
    assert(!(io.replay_s4.fire && PopCount(replayMatchVec) > 1.U), "replay_s4 match multiple buffers: %b txnID: %d", replayMatchVec, io.replay_s4.bits.txnID)

    def addrConflict(set: UInt, tag: UInt): Bool = {
        val mshrAddrConflict = VecInit(io.mshrStatus.map { case s =>
            s.valid && s.set === set && s.reqTag === tag && s.hasPendingRefill
        }).asUInt.orR

        mshrAddrConflict
    }

    buffers.zipWithIndex.zip(insertOH.asBools).foreach { case ((buf, i), en) =>
        when(en && io.taskIn.fire) {
            buf.state := SnpBufState.WAIT
            buf.task  := io.taskIn.bits
            assert(buf.state === SnpBufState.INVALID)
        }

        when(buf.state =/= SnpBufState.INVALID) {
            when(io.replay_s4.fire && io.replay_s4.bits.txnID === buf.task.txnID) {
                when(io.replay_s4.bits.shouldReplay) {
                    buf.state             := SnpBufState.WAIT
                    buf.task.isReplayTask := true.B
                }.otherwise {
                    buf.state := SnpBufState.INVALID
                }
            }

            val addrConflict_all = addrConflict(buf.task.set, buf.task.tag)

            // Update the ready signal for this buffer based on conflict checks
            buf.ready := !addrConflict_all
        }
    }

    // If the input has multiple valid, choose the one with the oldest age
    val inputValidVec = VecInit(buffers.map(buf => buf.state === SnpBufState.WAIT && buf.ready)).asUInt
    val maxAge        = ParallelMax(buffers.map(buf => buf.age))
    val inputMaskVec  = VecInit(inputValidVec.asBools.zipWithIndex.map { case (valid, i) => valid && buffers(i).age === maxAge }).asUInt

    issueArb.io.in.zipWithIndex.zip(buffers).foreach { case ((in, i), buf) =>
        in.valid := buf.state === SnpBufState.WAIT && buf.ready && inputMaskVec(i)
        in.bits  := buf.task

        when(in.fire) {
            buf.age   := 0.U
            buf.state := SnpBufState.SENT
            assert(buf.state === SnpBufState.WAIT, "buf.state => 0b%b", buf.state)

            val buffersExceptMe = buffers.zipWithIndex.filter(_._2 != i).collect(_._1)
            buffersExceptMe.foreach { buf =>
                when(buf.state === SnpBufState.WAIT && buf.ready && buf.age < (nrSnpBufEntry - 1).U) {
                    // The buffer has not been chosen as candidate for output, so its age is incremented
                    buf.age := buf.age + 1.U
                }
            }
        }
    }

    taskOut <> issueArb.io.out

    println(s"[${this.getClass().toString()}] snpBufOutLatch:${optParam.snpBufOutLatch}")

    if (optParam.snpBufOutLatch) {
        val chosenQ = Module(
            new Queue(
                new Bundle {
                    val task  = new TaskBundle
                    val bufId = UInt(log2Ceil(nrSnpBufEntry).W)
                },
                entries = 1,
                pipe = false, // TODO:
                flow = false
            )
        )
        val cancel = !buffers(chosenQ.io.deq.bits.bufId).ready

        chosenQ.io.enq.valid      := taskOut.valid
        chosenQ.io.enq.bits.task  := taskOut.bits
        chosenQ.io.enq.bits.bufId := issueArb.io.chosen
        taskOut.ready             := chosenQ.io.enq.ready

        chosenQ.io.deq.ready := io.taskOut.ready || cancel
        io.taskOut.valid     := chosenQ.io.deq.valid && !cancel
        io.taskOut.bits      := chosenQ.io.deq.bits.task

        when(chosenQ.io.deq.fire && cancel) {
            buffers(chosenQ.io.deq.bits.bufId).state := SnpBufState.WAIT
            assert(buffers(chosenQ.io.deq.bits.bufId).state === SnpBufState.SENT, "buf.state => 0b%b", buffers(chosenQ.io.deq.bits.bufId).state)
        }
    } else {
        io.taskOut <> taskOut
    }

    buffers.zipWithIndex.foreach { case (buf, i) =>
        LeakChecker(buf.state =/= SnpBufState.INVALID, buf.state === SnpBufState.INVALID, Some(s"buffers_valid_${i}"), maxCount = deadlockThreshold)
    }
}

object SnoopBuffer extends App {
    val config = SimpleL2.DefaultConfig()

    GenerateVerilog(args, () => new SnoopBuffer()(config), name = "SnoopBuffer", split = false)
}
