package SimpleL2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import Utils.{GenerateVerilog, LeakChecker}
import SimpleL2.Configs._
import SimpleL2.Bundles._
import SimpleL2.chi._

class TXRSP()(implicit p: Parameters) extends L2Module {
    val io = IO(new Bundle {
        val mpTask_s4 = Flipped(DecoupledIO(new CHIBundleRSP(chiBundleParams)))
        val mshrTask  = Flipped(DecoupledIO(new CHIBundleRSP(chiBundleParams)))
        val out       = DecoupledIO(new CHIBundleRSP(chiBundleParams))
        val txrspCnt  = Output(UInt(log2Ceil(nrTXRSPEntry + 1).W))
    })

    val queue = Module(new Queue(new CHIBundleRSP(chiBundleParams), nrTXRSPEntry))
    queue.io.enq.valid := io.mpTask_s4.valid || io.mshrTask.valid
    queue.io.enq.bits  := Mux(io.mpTask_s4.valid, io.mpTask_s4.bits, io.mshrTask.bits)

    io.mpTask_s4.ready := queue.io.enq.ready
    io.mshrTask.ready  := !io.mpTask_s4.valid && queue.io.enq.ready

    io.out      <> queue.io.deq
    io.txrspCnt := queue.io.count

    LeakChecker(io.out.valid, io.out.fire, Some("TXRSP_valid"), maxCount = deadlockThreshold)
}

object TXRSP extends App {
    val config = SimpleL2.DefaultConfig()

    GenerateVerilog(args, () => new TXRSP()(config), name = "TXRSP", split = false)
}
