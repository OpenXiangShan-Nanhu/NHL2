package SimpleL2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import SimpleL2.Configs._
import SimpleL2.Bundles._

object PowerState {
    val width = 2

    val ACTIVE     = "b00".U(width.W) // 0, stable
    val TRANSITION = "b01".U(width.W) // 1, unstable
    val SHUTDOWN   = "b10".U(width.W) // 2, stable
    val RETENTION  = "b11".U(width.W) // 3, stable
}

class LowPowerCtrl(implicit p: Parameters) extends L2Module {
    val io = IO(new Bundle {
        val lowPower = new LowPowerIO

        val mpStatus_s123  = Input(new MpStatus123)
        val mpStatus_s4567 = Input(new MpStatus4567)
        val mshrStatus     = Vec(nrMSHR, Input(new MshrStatus))

        /** Evict request sent to [[RequestArbiter]] */
        val toReqArb = DecoupledIO(new LowPowerToReqArb)

        val rxsnpValid       = Input(Bool())
        val retentionWakeup  = Input(Bool()) // Only snoop request can wakeup retention
        val sramWakeupFinish = Input(Bool())
        val powerState       = Output(UInt(PowerState.width.W))
        val lastPowerState   = Output(UInt(PowerState.width.W))
    })

    val SUCCESS = true.B
    val FAIL    = false.B

    val mpHasRequest = VecInit(io.mpStatus_s123.elements.map { case (_: String, stage: MpStageInfo) => stage.valid }.toSeq).asUInt.orR || VecInit(io.mpStatus_s4567.elements.map { case (_: String, stage: MpStageInfo) => stage.valid }.toSeq).asUInt.orR
    val mshrValidVec = VecInit(io.mshrStatus.map(_.valid)).asUInt
    val mshrValidCnt = PopCount(mshrValidVec)
    val mshrAllFree  = !mshrValidVec.orR

    val retentionWakeup_dly1 = RegNext(io.retentionWakeup, false.B)
    val retentionWakeup_dly2 = RegNext(retentionWakeup_dly1, false.B)
    val retentionWakeup_dly3 = RegNext(retentionWakeup_dly2, false.B)
    val retentionWakeupValid = io.retentionWakeup || retentionWakeup_dly1 || retentionWakeup_dly2 || retentionWakeup_dly3

    val powerState      = RegInit(PowerState.ACTIVE)
    val lastStableState = RegInit(PowerState.ACTIVE)
    val nextStableState = RegInit(PowerState.ACTIVE)
    val nextPowerState  = WireInit(PowerState.ACTIVE)

    val respState     = RegInit(SUCCESS)
    val wakeupBySnoop = RegInit(false.B)
    val mpGetSnoop    = RegInit(false.B)
    val evictFinish   = WireInit(false.B)
    val timer         = RegInit(0.U(log2Up(sramShutdownWakeupCycles + 1).W))

    dontTouch(powerState)
    dontTouch(nextPowerState)

    /** Power state transition FSM */
    nextPowerState := powerState

    switch(powerState) {
        is(PowerState.ACTIVE) {
            when(io.lowPower.isRetention) {
                when(mshrAllFree && !mpHasRequest) {
                    nextPowerState  := PowerState.RETENTION
                    lastStableState := PowerState.ACTIVE
                    nextStableState := PowerState.RETENTION
                    respState       := SUCCESS
                }.otherwise {
                    respState := FAIL
                }
            }.elsewhen(io.lowPower.isOff) {
                nextPowerState  := PowerState.TRANSITION
                lastStableState := PowerState.ACTIVE
                nextStableState := PowerState.SHUTDOWN
            }.elsewhen(lastStableState === PowerState.RETENTION && wakeupBySnoop) {
                when(!mpGetSnoop) {
                    mpGetSnoop := io.mpStatus_s123.stage2.valid // TODO: Check if this request is a snoop request
                }.otherwise {
                    when(mshrAllFree && !mpHasRequest) {
                        nextPowerState  := PowerState.RETENTION
                        lastStableState := PowerState.ACTIVE
                        nextStableState := PowerState.RETENTION
                    }
                }
            }
        }

        is(PowerState.SHUTDOWN) {
            when(io.lowPower.isOn) {
                nextPowerState  := PowerState.TRANSITION
                lastStableState := PowerState.SHUTDOWN
                nextStableState := PowerState.ACTIVE
            }
            assert(!io.lowPower.isRetention, "Power mode cannot transfer from SHUTDOWN to RETENTION")
        }

        is(PowerState.RETENTION) {
            when(io.lowPower.isOn) {
                nextPowerState  := PowerState.TRANSITION
                lastStableState := PowerState.RETENTION
                nextStableState := PowerState.ACTIVE
                wakeupBySnoop   := false.B
            }.elsewhen(io.retentionWakeup) {
                nextPowerState  := PowerState.TRANSITION
                lastStableState := PowerState.RETENTION
                nextStableState := PowerState.ACTIVE
                wakeupBySnoop   := true.B
            }
            assert(!io.lowPower.isOff && !io.lowPower.isRetention, "Power mode cannot transfer from RETENTION to OFF/RETENTION")
        }

        is(PowerState.TRANSITION) {
            when(lastStableState === PowerState.RETENTION) {
                when(io.sramWakeupFinish) {
                    nextPowerState := PowerState.ACTIVE
                    assert(nextPowerState === nextStableState)
                }
            }.elsewhen(lastStableState === PowerState.ACTIVE) {
                when(evictFinish) {
                    nextPowerState := PowerState.SHUTDOWN
                    assert(nextPowerState === nextStableState)
                }
            }.elsewhen(lastStableState === PowerState.SHUTDOWN) {
                when(timer === sramShutdownWakeupCycles.U) {
                    timer          := 0.U
                    nextPowerState := PowerState.ACTIVE
                    assert(nextPowerState === nextStableState)
                }.otherwise {
                    timer := timer + 1.U
                }
            }
            assert(!io.lowPower.req.fire, "Power mode is transiting...")
        }
    }

    powerState        := nextPowerState
    io.powerState     := powerState
    io.lastPowerState := lastStableState
    io.lowPower.idle  := mshrAllFree && !mpHasRequest // TODO: Check for SourceD and TXDAT

    // Send lower power response according to the current state of the l2cache
    val reqValid_s1 = io.lowPower.req.fire
    val reqIsOff_s2 = RegEnable(io.lowPower.isOff, reqValid_s1) // Send response for the OFF request when the eviction is finished
    val reqIsOn_s2  = RegEnable(io.lowPower.isOn, reqValid_s1)
    val reqValid_s2 = RegNext(reqValid_s1, false.B)             // powerState === PowerState.TRANSITION && lastStableState === PowerState.SHUTDOWN && nextPowerState === PowerState.ACTIVE
    io.lowPower.resp.valid := Mux(
        reqIsOff_s2,
        evictFinish,
        Mux(
            reqIsOn_s2 && powerState === PowerState.TRANSITION,
            nextPowerState === PowerState.ACTIVE,
            reqValid_s2
        )
    )
    io.lowPower.resp.bits := Mux(reqIsOff_s2 || reqIsOff_s2, SUCCESS, respState)

    val MSHR_FREE_THRESHOLD = lowPowerMSHRFreeThreshold
    val startTimer          = RegInit(0.U(log2Up(MSHR_FREE_THRESHOLD + 1).W))
    val enableEvict         = RegInit(false.B)

    when(lastStableState === PowerState.ACTIVE && powerState === PowerState.TRANSITION && nextStableState === PowerState.SHUTDOWN && mshrAllFree && startTimer < MSHR_FREE_THRESHOLD.U) {
        startTimer := startTimer + 1.U
    }

    /** When [[MSHR]]s are all free for a long time, enable eviction */
    when(startTimer >= MSHR_FREE_THRESHOLD.U) {
        enableEvict := true.B
    }.otherwise {
        enableEvict := false.B
    }

    /** Iterate over all sets and ways to evict every valid cacheline */
    val setCnt   = RegInit(0.U(setBits.W))
    val wayCnt   = RegInit(0.U(wayBits.W))
    val mshrIsOk = mshrValidCnt <= (nrMSHR / 2).U // Only part of MSHRs are used for eviction, this will reduce unnecessary backpressure signals from MSHR
    evictFinish := setCnt >= (sets - 1).U && wayCnt >= (ways - 1).U

    io.toReqArb.valid       := enableEvict && mshrIsOk && !evictFinish
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

    when(io.lowPower.isOff || evictFinish) {
        setCnt     := 0.U
        wayCnt     := 0.U
        startTimer := 0.U
    }
}
