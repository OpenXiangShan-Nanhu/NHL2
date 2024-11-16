package SimpleL2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import SimpleL2.Configs._
import SimpleL2.Bundles._

object PowerState {
    val width = 2

    val ACTIVE     = "b00".U(width.W) // stable
    val TRANSITION = "b01".U(width.W) // unstable
    val SHUTDOWN   = "b10".U(width.W) // stable
    val RETENTION  = "b11".U(width.W) // stable
}

class LowPowerCtrl(implicit p: Parameters) extends L2Module {
    val io = IO(new Bundle {
        val lowPower = new LowPowerIO

        val mpStatus_s123  = Input(new MpStatus123)
        val mpStatus_s4567 = Input(new MpStatus4567)
        val mshrStatus     = Vec(nrMSHR, Input(new MshrStatus))

        /** Evict request sent to [[RequestArbiter]] */
        val toReqArb = DecoupledIO(new LowPowerToReqArb)

        val rxsnpValid      = Input(Bool())
        val retentionWakeup = Input(Bool()) // Only snoop request can wakeup retention
        val powerState      = Output(UInt(PowerState.width.W))
    })

    assert(!(io.lowPower.shutdown.req && io.lowPower.retention.req), "Only shutdown or retention can be active at the same time!")

    val shutdown       = io.lowPower.shutdown
    val retention      = io.lowPower.retention
    val powerState     = RegInit(PowerState.ACTIVE)
    val nextPowerState = WireInit(PowerState.ACTIVE)

    /** Power state transition FSM */
    nextPowerState := powerState

    switch(powerState) {
        is(PowerState.ACTIVE) {
            when((retention.req ^ retention.ack) || (shutdown.req ^ shutdown.ack)) {
                nextPowerState := PowerState.TRANSITION
            }
        }
        is(PowerState.TRANSITION) {
            when(retention.req && retention.ack) {
                // ACTIVE -> TRANSITION -> RETENTION
                nextPowerState := PowerState.RETENTION
            }.elsewhen(shutdown.req && shutdown.ack) {
                // ACTIVE -> TRANSITION -> SHUTDOWN
                nextPowerState := PowerState.SHUTDOWN
            }.elsewhen(!retention.req && !retention.ack) {
                // RETENTION -> TRANSITION -> ACTIVE
                nextPowerState := PowerState.ACTIVE
            }

            assert(!(shutdown.ack && retention.ack))
        }
        is(PowerState.SHUTDOWN) {
            // do nothing for now
        }
        is(PowerState.RETENTION) {
            when(retention.req ^ retention.ack) {
                nextPowerState := PowerState.TRANSITION
            }
            assert(!shutdown.req, "retention can not be active at the same time with shutdown")
        }
    }

    powerState    := nextPowerState
    io.powerState := powerState

    dontTouch(powerState)
    dontTouch(nextPowerState)

    val lowPowerShutdownReq  = RegNext(io.lowPower.shutdown.req, false.B)
    val lowPowerShutdownAck  = RegInit(false.B)
    val lowPowerRetentionReq = RegNext(io.lowPower.retention.req, false.B)
    val lowPowerRetentionAck = RegInit(false.B)

    val MSHR_FREE_THRESHOLD = lowPowerMSHRFreeThreshold
    val startTimer          = RegInit(0.U(log2Up(MSHR_FREE_THRESHOLD + 1).W))
    val enableEvict         = RegInit(false.B)
    def mshrIsEmpty = enableEvict

    val mshrValidVec = VecInit(io.mshrStatus.map(_.valid)).asUInt
    val mshrValidCnt = PopCount(mshrValidVec)
    val mshrAllFree  = !mshrValidVec.orR

    when((lowPowerShutdownReq || lowPowerRetentionReq) && mshrAllFree && startTimer < MSHR_FREE_THRESHOLD.U) {
        startTimer := startTimer + 1.U
    }.elsewhen(!lowPowerShutdownReq && !lowPowerRetentionReq) {
        startTimer := 0.U
    }

    /** When [[MSHR]]s are all free for a long time, enable eviction */
    when(startTimer >= MSHR_FREE_THRESHOLD.U) {
        enableEvict := true.B
    }.otherwise {
        enableEvict := false.B
    }

    /** Iterate over all sets and ways to evict every valid cacheline */
    val setCnt      = RegInit(0.U(setBits.W))
    val wayCnt      = RegInit(0.U(wayBits.W))
    val evictFinish = setCnt >= (sets - 1).U && wayCnt >= (ways - 1).U
    val mshrIsOk    = mshrValidCnt <= (nrMSHR / 2).U // Only part of MSHRs are used for eviction, this will reduce unnecessary backpressure signals from MSHR

    io.toReqArb.valid       := lowPowerShutdownReq && enableEvict && mshrIsOk && !evictFinish
    io.toReqArb.bits.set    := setCnt
    io.toReqArb.bits.wayIdx := wayCnt

    when(io.toReqArb.fire && !evictFinish) {
        when(wayCnt >= (ways - 1).U) {
            setCnt := setCnt + 1.U
            wayCnt := 0.U
        }.otherwise {
            wayCnt := wayCnt + 1.U
        }
    }

    when(!lowPowerShutdownReq && RegNext(lowPowerShutdownReq)) {
        setCnt := 0.U
        wayCnt := 0.U
    }

    lowPowerShutdownAck  := evictFinish && lowPowerShutdownReq
    lowPowerRetentionAck := mshrIsEmpty && lowPowerRetentionReq

    io.lowPower.shutdown.ack  := lowPowerShutdownAck
    io.lowPower.retention.ack := lowPowerRetentionAck

    val mpHasRequest         = VecInit(io.mpStatus_s123.elements.map { case (_: String, stage: MpStageInfo) => stage.valid }.toSeq).asUInt.orR || VecInit(io.mpStatus_s4567.elements.map { case (_: String, stage: MpStageInfo) => stage.valid }.toSeq).asUInt.orR
    val retentionWakeup_dly1 = RegNext(io.retentionWakeup, false.B)
    val retentionWakeup_dly2 = RegNext(retentionWakeup_dly1, false.B)
    val retentionWakeup_dly3 = RegNext(retentionWakeup_dly2, false.B)
    val retentionWakeupValid = io.retentionWakeup || retentionWakeup_dly1 || retentionWakeup_dly2 || retentionWakeup_dly3

    val lastPowerStateIsRetention = RegInit(false.B)

    when(powerState === PowerState.RETENTION && nextPowerState === PowerState.TRANSITION) {
        lastPowerStateIsRetention := true.B
    }.elsewhen(powerState === PowerState.ACTIVE && nextPowerState === PowerState.TRANSITION) {
        lastPowerStateIsRetention := false.B
    }

    io.lowPower.retention.rdy := !retentionWakeupValid && Mux(lastPowerStateIsRetention, mshrAllFree && !mpHasRequest && !io.rxsnpValid, true.B)
}
