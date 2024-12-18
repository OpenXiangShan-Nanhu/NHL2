package SimpleL2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink._
import Utils.{GenerateVerilog, LeakChecker}
import SimpleL2.Configs._
import SimpleL2.Bundles._
import xs.utils.tl.{TLNanhuBusKey, TLNanhuUserBundle}

class SinkA()(implicit p: Parameters) extends L2Module {
    val io = IO(new Bundle {
        val a               = Flipped(DecoupledIO(new TLBundleA(tlBundleParams)))
        val prefetchReqOpt  = if (enablePrefetch) Some(Flipped(DecoupledIO(new SimpleL2.prefetch.PrefetchReq))) else None
        val task            = DecoupledIO(new TaskBundle)
        val sliceId         = Input(UInt(bankBits.W))
        val amoDataBufWrOpt = if (enableBypassAtomic) Some(Flipped(new AtomicDataBufferWrite)) else None
    })

    io      <> DontCare
    io.a    <> DontCare
    io.task <> DontCare

    val isCMOReq        = io.a.bits.opcode === TLMessages.Hint && io.a.bits.param =/= 0.U && enableBypassCMO.B
    val isAtomicReq     = io.a.bits.opcode === TLMessages.ArithmeticData || io.a.bits.opcode === TLMessages.LogicalData
    val amoDataBufReady = if (enableBypassAtomic) io.amoDataBufWrOpt.get.ready else true.B

    if (enablePrefetch) {
        val (tag, set, offset) = parseAddress(io.a.bits.address)
        assert(!(io.a.fire && !isAtomicReq && offset =/= 0.U))

        io.task.valid        := io.a.valid && Mux(isAtomicReq, amoDataBufReady, true.B) || io.prefetchReqOpt.get.valid
        io.task.bits.channel := L2Channel.ChannelA

        when(io.a.valid) {
            io.task.bits.opcode := io.a.bits.opcode
            io.task.bits.param  := io.a.bits.param
            io.task.bits.source := io.a.bits.source
            io.task.bits.set    := set
            io.task.bits.tag    := tag
            val userField = io.a.bits.user.lift(TLNanhuBusKey).getOrElse(0.U.asTypeOf(new TLNanhuUserBundle))
            io.task.bits.vaddrOpt.foreach(_ := userField.vaddr.getOrElse(0.U))
            io.task.bits.needHintOpt.foreach(_ := userField.pfHint)
            io.task.bits.aliasOpt.foreach(_ := userField.alias.getOrElse(0.U))
        }.otherwise {
            val req    = io.prefetchReqOpt.get.bits
            val bankId = req.set(bankBits - 1, 0)
            assert(!(io.prefetchReqOpt.get.fire && bankId =/= io.sliceId), "[prefetchReq] bankId:%d =/= sliceId:%d", bankId, io.sliceId)

            io.task.bits.opcode := TLMessages.Hint
            io.task.bits.param  := Mux(req.needT, TLHints.PREFETCH_WRITE, TLHints.PREFETCH_READ)
            io.task.bits.source := req.source
            io.task.bits.set    := req.set >> bankBits.U
            io.task.bits.tag    := req.tag

            io.task.bits.vaddrOpt.foreach(_ := req.vaddr.getOrElse(0.U))
            io.task.bits.needHintOpt.foreach(_ := false.B)
            io.task.bits.aliasOpt.foreach(_ := 0.U)
        }

        io.a.ready := io.task.ready && Mux(isAtomicReq, amoDataBufReady, true.B)
        when(!isAtomicReq && !isCMOReq) {
            assert(!(io.a.fire && io.a.bits.size =/= log2Ceil(blockBytes).U), "size:%d", io.a.bits.size)
        }
        LeakChecker(io.a.valid, io.a.fire, Some("SinkA_io_a_valid"), maxCount = deadlockThreshold)

        io.prefetchReqOpt.foreach { req =>
            req.ready := io.task.ready && !io.a.valid
            LeakChecker(req.valid, req.fire, Some("prefetch_valid"), maxCount = deadlockThreshold)
        }
    } else {
        val (tag, set, offset) = parseAddress(io.a.bits.address)
        assert(!(io.a.fire && offset =/= 0.U))

        io.task.valid        := io.a.valid && Mux(isAtomicReq, amoDataBufReady, true.B)
        io.task.bits.channel := L2Channel.ChannelA
        io.task.bits.opcode  := io.a.bits.opcode
        io.task.bits.param   := io.a.bits.param
        io.task.bits.source  := io.a.bits.source
        io.task.bits.set     := set
        io.task.bits.tag     := tag
        val userField = io.a.bits.user.lift(TLNanhuBusKey).getOrElse(0.U.asTypeOf(new TLNanhuUserBundle))
        io.task.bits.vaddrOpt.foreach(_ := userField.vaddr.getOrElse(0.U))
        io.task.bits.needHintOpt.foreach(_ := userField.pfHint)
        io.task.bits.aliasOpt.foreach(_ := userField.alias.getOrElse(0.U))

        io.a.ready := io.task.ready && Mux(isAtomicReq, amoDataBufReady, true.B)

        when(!isAtomicReq && !isCMOReq) {
            assert(!(io.a.fire && io.a.bits.size =/= log2Ceil(blockBytes).U), "size:%d", io.a.bits.size)
        }

        LeakChecker(io.a.valid, io.a.fire, Some("SinkA_io_a_valid"), maxCount = deadlockThreshold)
    }

    if (enableBypassAtomic) {
        val amoDataBufWr   = io.amoDataBufWrOpt.get
        val (_, _, offset) = parseAddress(io.a.bits.address)

        amoDataBufWr.valid           := io.a.fire && isAtomicReq
        amoDataBufWr.bits.data       := io.a.bits.data
        io.task.bits.amoBufIdOpt.get := amoDataBufWr.idx
        io.task.bits.offsetOpt.get   := offset
        io.task.bits.sizeOpt.get     := io.a.bits.size
        io.task.bits.maskOpt.get     := io.a.bits.mask
    }

    dontTouch(io)
}

object SinkA extends App {
    val config = SimpleL2.DefaultConfig()

    GenerateVerilog(args, () => new SinkA()(config), name = "SinkA", split = false)
}
