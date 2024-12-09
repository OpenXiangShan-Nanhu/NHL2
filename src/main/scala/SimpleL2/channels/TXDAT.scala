package SimpleL2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import Utils.{GenerateVerilog, SkidBuffer, LeakChecker}
import SimpleL2.Configs._
import SimpleL2.Bundles._
import SimpleL2.chi._
import SimpleL2.chi.CHIOpcodeDAT._

/**
 * Scenerio:
 *     CopyBackWrData Data from TempDataStorage (bypass data)
 *     CopyBackWrData Data from DataStorage (directly write-back data, there are not sub-requsets to process)
 *     CopyBackWrData Request from MainPipe (directly write-back, there are not sub-requsets to process)
 *     CopyBackWrData
 *  Request from MSHR (indirectly write-back, the probe needs to be completed first)
 *     SnpRespData Data from TempDataStorage (bypass data)
 *     SnpRespData Data from DataStorage
 *     SnpRespData Request from MainPipe
 *     SnpRespData Request from MSHR (mshr can be nested by other Snoop* requests, therefore SnpRespData needs to be sent)
 */
class TXDAT()(implicit p: Parameters) extends L2Module {
    val io = IO(new Bundle {
        val task_s2   = Flipped(DecoupledIO(new CHIBundleDAT(chiBundleParams)))
        val data_s2   = Flipped(DecoupledIO(UInt(dataBits.W)))
        val task_s7s8 = Flipped(DecoupledIO(new CHIBundleDAT(chiBundleParams)))
        val data_s7s8 = Flipped(DecoupledIO(UInt(dataBits.W)))
        val out       = DecoupledIO(new CHIBundleDAT(chiBundleParams))
    })

    val skidBuffer = Module(new SkidBuffer(new Bundle {
        val txdat = new CHIBundleDAT(chiBundleParams)
        val data  = UInt(dataBits.W)
    }))

    val txdat = Mux(io.task_s7s8.valid, io.task_s7s8.bits, io.task_s2.bits)
    io.task_s2.ready   := skidBuffer.io.enq.ready && !io.task_s7s8.valid
    io.task_s7s8.ready := skidBuffer.io.enq.ready

    io.data_s2.ready   := skidBuffer.io.enq.ready && !io.data_s7s8.valid
    io.data_s7s8.ready := skidBuffer.io.enq.ready

    skidBuffer.io.enq.valid      := io.task_s2.valid || io.task_s7s8.valid
    skidBuffer.io.enq.bits.txdat := txdat
    skidBuffer.io.enq.bits.data  := Mux(io.task_s7s8.valid, io.data_s7s8.bits, io.data_s2.bits)

    val isValidTXDAT_s2   = !(io.task_s2.bits.resp === Resp.I && io.task_s2.bits.opcode === CopyBackWrData) && io.task_s2.fire
    val isValidTXDAT_s7s8 = !(io.task_s7s8.bits.resp === Resp.I && io.task_s7s8.bits.opcode === CopyBackWrData) && io.task_s7s8.fire
    when(isValidTXDAT_s2 || isValidTXDAT_s7s8) {
        assert(!(io.task_s2.fire && !io.data_s2.fire), "data_s2 should arrive with task_s2!")
        assert(!(io.data_s2.fire && !io.task_s2.fire), "task_s2 should arrive with data_s2!")
        assert(!(io.task_s7s8.fire && !io.data_s7s8.fire), "data_s7s8 should arrive with task_s7s8!")
        assert(!(io.data_s7s8.fire && !io.task_s7s8.fire), "task_s7s8 should arrive with data_s7s8!")
        assert(!(io.task_s2.valid ^ io.data_s2.valid), "task_s2 should be valid with data_s2 valid!")
        assert(!(io.task_s7s8.valid ^ io.data_s7s8.valid), "task_s7s8 should be valid with task_s7s8 valid!")
    }

    val deq     = skidBuffer.io.deq
    val deqData = skidBuffer.io.deq.bits.data
    val beatCnt = RegInit(0.U(log2Ceil(nrBeat).W))
    val last    = beatCnt === (nrBeat - 1).U

    // If the cache line state is UC or SC after the Snoop response is sent, a Request Node is permitted to not send
    // valid CopyBack Data. If the Request Node decides not to send valid CopyBack Data, the cache state in the
    // CopyBackWriteData or CompAck response must be I. Additionally, for a CopyBackWriteData response, all
    // BE bits must be deasserted, and the corresponding data must be set to 0.
    val deqIsValid = !(deq.bits.txdat.resp === Resp.I && deq.bits.txdat.opcode === CHIOpcodeDAT.CopyBackWrData)

    when(io.out.fire && io.out.bits.opcode =/= CHIOpcodeDAT.NonCopyBackWrData) {
        when(last) {
            beatCnt := 0.U
        }.otherwise {
            beatCnt := beatCnt + 1.U
        }
    }

    deq.ready          := io.out.ready && Mux(io.out.bits.opcode === CHIOpcodeDAT.NonCopyBackWrData, true.B, last) // For now, NonCopyBackWrData only require one beat
    io.out.valid       := deq.valid
    io.out.bits        := deq.bits.txdat
    io.out.bits.data   := Mux(deqIsValid, Mux(last, deqData(511, 256), deqData(255, 0)), 0.U)
    io.out.bits.dataID := Mux(last, "b10".U, "b00".U)
    io.out.bits.be     := Mux(deqIsValid, deq.bits.txdat.be, 0.U)

    LeakChecker(io.out.valid, io.out.fire, Some("TXDAT_valid"), maxCount = deadlockThreshold)
}
