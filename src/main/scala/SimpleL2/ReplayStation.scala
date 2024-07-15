package SimpleL2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils.perf.{DebugOptions, DebugOptionsKey}
import Utils.GenerateVerilog
import SimpleL2.chi.Resp
import SimpleL2.Configs._
import SimpleL2.Bundles._
import freechips.rocketchip.util.SeqToAugmentedSeq
import Utils.LFSRArbiter

object ReplayResion {
    val width            = 3
    val NoFreeMSHR       = "b001".U(width.W)
    val NoDataBufSourceD = "b010".U(width.W)
    val NoDataBufTXDAT   = "b011".U(width.W)
    val NoSpaceTXRSP     = "b100".U(width.W)
    val BufferRequest    = "b101".U(width.W)
}

class ReplayRequest(implicit p: Parameters) extends L2Bundle {
    val task   = new TaskBundle
    val reason = UInt(ReplayResion.width.W)
}

class ReplaySubEntry(implicit p: Parameters) extends L2Bundle {
    val channel     = UInt(L2Channel.width.W)
    val isCHIOpcode = Bool()
    val opcode      = UInt(setBits.W)
    val param       = UInt(math.max(3, Resp.width).W)                 // if isCHIOpcode is true, param is equals to the resp field in CHI
    val source      = UInt(math.max(tlBundleParams.sourceBits, 12).W) // CHI RXRSP TxnID ==> 12.W, if isCHIOpcode is true, source is equals to the resp field in CHI
    val aliasOpt    = aliasBitsOpt.map(width => UInt(width.W))
    val isAliasTask = Bool()
    val retToSrc    = Bool()

    def resp = param
    def txnID = source     // alias to source
    def chiOpcode = opcode // alias to opcode
}
class ReplayEntry(implicit p: Parameters) extends L2Bundle {
    val set        = UInt(setBits.W)
    val tag        = UInt(tagBits.W)
    val subEntries = Vec(nrClients + 1, Valid(new ReplaySubEntry))
    val enqIdx     = new Counter(nrClients + 1)
    val deqIdx     = new Counter(nrClients + 1)

    def subValidVec = VecInit(subEntries.map(_.valid)).asUInt
}

class ReplayStation()(implicit p: Parameters) extends L2Module {
    val io = IO(new Bundle {
        val replay_s4 = Flipped(ValidIO(new ReplayRequest))
        val req_s1    = DecoupledIO(new TaskBundle)
        val freeCnt   = Output(UInt((log2Ceil(nrReplayEntry) + 1).W))
    })

    io <> DontCare

    val entries = Seq.fill(nrReplayEntry) { RegInit(0.U.asTypeOf(Valid(new ReplayEntry))) }
    val freeVec = VecInit(entries.map(!_.valid)).asUInt
    val freeOH  = PriorityEncoderOH(freeVec)
    val freeIdx = OHToUInt(freeOH)

    entries.foreach(dontTouch(_))

    val validVec    = VecInit(entries.map(_.valid)).asUInt
    val matchSetVec = VecInit(entries.map(_.bits.set === io.replay_s4.bits.task.set)).asUInt
    val matchTagVec = VecInit(entries.map(_.bits.tag === io.replay_s4.bits.task.tag)).asUInt
    val matchVec    = validVec & matchSetVec & matchTagVec
    val hasMatch    = matchVec.orR
    assert(PopCount(matchVec) <= 1.U, "Multiple match")

    when(io.replay_s4.fire) {
        when(hasMatch) {
            entries.zip(matchVec.asBools).foreach { case (entry, en) =>
                when(en) {
                    val subEntry = entry.bits.subEntries(entry.bits.enqIdx.value)
                    subEntry.valid            := true.B
                    subEntry.bits.isCHIOpcode := io.replay_s4.bits.task.isCHIOpcode
                    subEntry.bits.opcode      := io.replay_s4.bits.task.opcode
                    subEntry.bits.param       := io.replay_s4.bits.task.param
                    subEntry.bits.channel     := io.replay_s4.bits.task.channel
                    subEntry.bits.source      := io.replay_s4.bits.task.source
                    subEntry.bits.retToSrc    := io.replay_s4.bits.task.retToSrc
                    subEntry.bits.isAliasTask := io.replay_s4.bits.task.isAliasTask
                    subEntry.bits.aliasOpt.foreach(_ := io.replay_s4.bits.task.aliasOpt.get)

                    entry.bits.enqIdx.inc()

                    assert(!subEntry.valid)
                    assert(entry.valid)
                }
            }
        }.otherwise {
            assert(freeVec.orR, "No free replay entry")

            val entry    = entries(freeIdx)
            val subEntry = entry.bits.subEntries.head
            entries.zip(freeOH.asBools).zipWithIndex.foreach { case ((entry, en), i) =>
                when(en) {
                    val subEntry = entry.bits.subEntries(entry.bits.enqIdx.value)
                    entry.valid               := true.B
                    entry.bits.set            := io.replay_s4.bits.task.set
                    entry.bits.tag            := io.replay_s4.bits.task.tag
                    subEntry.valid            := true.B
                    subEntry.bits.isCHIOpcode := io.replay_s4.bits.task.isCHIOpcode
                    subEntry.bits.opcode      := io.replay_s4.bits.task.opcode
                    subEntry.bits.param       := io.replay_s4.bits.task.param
                    subEntry.bits.channel     := io.replay_s4.bits.task.channel
                    subEntry.bits.source      := io.replay_s4.bits.task.source
                    subEntry.bits.retToSrc    := io.replay_s4.bits.task.retToSrc
                    subEntry.bits.isAliasTask := io.replay_s4.bits.task.isAliasTask
                    subEntry.bits.aliasOpt.foreach(_ := io.replay_s4.bits.task.aliasOpt.get)

                    entry.bits.enqIdx.inc()
                }
            }
            assert(!entry.valid)
            assert(!subEntry.valid)
        }
    }

    val lfsrArb = Module(new LFSRArbiter(new TaskBundle, nrReplayEntry))
    lfsrArb.io.in.zipWithIndex.foreach { case (in, i) =>
        val entry    = entries(i)
        val deqOH    = UIntToOH(entry.bits.deqIdx.value)
        val subEntry = Mux1H(deqOH, entry.bits.subEntries)
        // val subEntry = entry.bits.subEntries(entry.bits.deqIdx.value)
        in.valid            := entry.valid && subEntry.valid
        in.bits             := DontCare
        in.bits.set         := entry.bits.set
        in.bits.tag         := entry.bits.tag
        in.bits.isCHIOpcode := subEntry.bits.isCHIOpcode
        in.bits.opcode      := subEntry.bits.opcode
        in.bits.param       := subEntry.bits.param
        in.bits.channel     := subEntry.bits.channel
        in.bits.source      := subEntry.bits.source
        in.bits.retToSrc    := subEntry.bits.retToSrc
        in.bits.isAliasTask := subEntry.bits.isAliasTask
        in.bits.aliasOpt.foreach(_ := subEntry.bits.aliasOpt.get)
        when(in.fire) {
            entry.bits.deqIdx.inc()
            when(PopCount(entry.bits.subValidVec) === 1.U) {
                entry.valid := false.B
            }
        }
    }

    when(lfsrArb.io.out.fire) {
        entries.zipWithIndex.foreach { case (entry, i) =>
            when(i.U === lfsrArb.io.chosen) {
                val deqSubEntry = entry.bits.subEntries(entry.bits.deqIdx.value)
                deqSubEntry.valid := false.B

                assert(deqSubEntry.valid)
                assert(entry.valid)
            }
        }
    }

    io.req_s1 <> lfsrArb.io.out

    io.freeCnt := PopCount(freeVec)

    dontTouch(io)
}

object ReplayStation extends App {
    val config = new Config((_, _, _) => {
        case L2ParamKey      => L2Param()
        case DebugOptionsKey => DebugOptions()
    })

    GenerateVerilog(args, () => new ReplayStation()(config), name = "ReplayStation", split = false)
}
