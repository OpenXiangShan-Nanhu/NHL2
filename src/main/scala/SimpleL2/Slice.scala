package SimpleL2

import chisel3._
import chisel3.util._
import chisel3.experimental.SourceInfo
import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import xs.utils.perf._
import xs.utils.tl.{ReqSourceKey, TLNanhuBusKey, TLNanhuUserBundle, TLUserKey, TLUserParams}
import Utils.{GenerateVerilog, IDPool}
import SimpleL2.Configs._
import SimpleL2.Bundles._
import SimpleL2.chi._

/**
  * CHI request retry info.
  * [[Slice]] should provide this info to the top-level IO.
  * The L2 top will use this info to match the PCrdGrant request with the proper [[Slice]] that is waiting for this PCrdGrant.
  */
class PCrdRetryInfo(implicit p: Parameters) extends L2Bundle {
    val valid    = Bool()
    val srcID    = UInt(chiBundleParams.nodeIdBits.W)
    val pCrdType = UInt(4.W)
}

class Slice()(implicit p: Parameters) extends L2Module {
    val io = IO(new Bundle {
        val tl          = Flipped(TLBundle(tlBundleParams))
        val chi         = CHIBundleDecoupled(chiBundleParams)
        val chiLinkCtrl = new CHILinkCtrlIO()
        val sliceId     = Input(UInt(bankBits.W))
        val eccError    = Output(Bool())

        val pCrdRetryInfoVec = Output(Vec(nrMSHR, new PCrdRetryInfo))

        val prefetchReqOpt   = if (enablePrefetch) Some(Flipped(DecoupledIO(new SimpleL2.prefetch.PrefetchReq))) else None
        val prefetchTrainOpt = if (enablePrefetch) Some(DecoupledIO(new SimpleL2.prefetch.PrefetchTrain)) else None
        val prefetchRespOpt  = if (enablePrefetch) Some(DecoupledIO(new PrefetchRespWithSource(tlBundleParams.sourceBits))) else None

        val lowPowerOpt = if (hasLowPowerInterface) Some(new LowPowerIO) else None
    })

    println(s"[${this.getClass().toString()}] addressBits:$addressBits")
    println(s"[${this.getClass().toString()}] tagBits:$tagBits")
    println(s"[${this.getClass().toString()}] setBits:$setBits")
    println(s"[${this.getClass().toString()}] bankBits:$bankBits")
    println(s"[${this.getClass().toString()}] offsetBits:$offsetBits")
    println(s"[${this.getClass().toString()}] enablePrefetch:$enablePrefetch")
    println(s"[${this.getClass().toString()}] supportDCT:${supportDCT}")
    println(s"[${this.getClass().toString()}] optParam:${optParam}")
    println(s"[${this.getClass().toString()}] TaskBundle bits:${(new TaskBundle).getWidth}")

    if (l2param.useDiplomacy) {
        val _nrClients = edgeIn.client.clients.count(_.supports.probe)
        require(
            _nrClients == nrClients,
            s"Number of Diplomatic clients (${_nrClients}) does not match number of client caches (${l2param.nrClients})"
        )
    }

    io.tl          <> DontCare
    io.chi         <> DontCare
    io.chiLinkCtrl <> DontCare

    /** TileLink side channels (upstream) */
    val sinkA   = Module(new SinkA)
    val sinkC   = Module(new SinkC)
    val sinkE   = Module(new SinkE)
    val sourceD = Module(new SourceD)
    val sourceB = Module(new SourceB)

    /** CHI side channels (downstream) */
    val txreq = Module(new TXREQ)
    val txrsp = Module(new TXRSP)
    val txdat = Module(new TXDAT)
    val rxdat = Module(new RXDAT)
    val rxrsp = Module(new RXRSP)
    val rxsnp = Module(new RXSNP)

    /** Other modules */
    val reqArb      = Module(new RequestArbiter)
    val dir         = Module(new Directory)
    val ds          = Module(new DataStorage)
    val mainPipe    = Module(new MainPipe)
    val tempDS      = Module(new TempDataStorage)
    val missHandler = Module(new MissHandler)
    val snpBuf      = Module(new SnoopBuffer)

    // TODO: sinkIdPool backpressure when full
    val sinkIdPool = Module(new IDPool((nrMSHR until (nrMSHR + nrExtraSinkId)).toSet))

    val amoDataBufOpt = if (enableBypassAtomic) Some(Module(new AtomicDataBuffer)) else None

    sinkIdPool.io.alloc.valid    := sourceD.io.sinkIdAlloc.valid
    sourceD.io.sinkIdAlloc.idOut := sinkIdPool.io.alloc.idOut
    sinkIdPool.io.free.valid     := sinkE.io.sinkIdFree.valid
    sinkIdPool.io.free.idIn      := sinkE.io.sinkIdFree.idIn

    sinkA.io.a <> io.tl.a
    sinkC.io.c <> io.tl.c
    sinkE.io.e <> io.tl.e

    val reqArbTaskSinkA = WireInit(0.U.asTypeOf(reqArb.io.taskSinkA_s1))

    if (!optParam.sinkaStallOnReqArb) {
        val reqBuf = Module(new RequestBufferV2)

        reqBuf.io.taskIn          <> sinkA.io.task
        reqBuf.io.mpStatus_s123   <> reqArb.io.status
        reqBuf.io.mpStatus_s45678 <> mainPipe.io.status
        reqBuf.io.mshrStatus      <> missHandler.io.mshrStatus
        reqBuf.io.bufferStatus    := sourceD.io.bufferStatus
        reqBuf.io.replay_s4       <> mainPipe.io.reqBufReplay_s4_opt.getOrElse(DontCare)

        reqArb.io.replayFreeCntSinkA := DontCare

        reqArbTaskSinkA <> reqBuf.io.taskOut
    } else {
        val replayStationSinkA = Module(new ReplayStation(nrReplayEntry = nrReplayEntrySinkA, nrSubEntry = nrClients + 1)) // for SinkA
        val reqBuf             = Module(new RequestBuffer)

        reqBuf.io.taskIn                      <> sinkA.io.task
        reqBuf.io.mshrStatus                  <> missHandler.io.mshrStatus
        replayStationSinkA.io.replay_s4       <> mainPipe.io.replayOpt_s4.get
        replayStationSinkA.io.replay_s4.valid := mainPipe.io.replayOpt_s4.get.valid && mainPipe.io.replayOpt_s4.get.bits.task.isChannelA
        replayStationSinkA.io.replay_s4.bits  := mainPipe.io.replayOpt_s4.get.bits

        reqArb.io.replayFreeCntSinkA := replayStationSinkA.io.freeCnt
        arbTask(Seq(Queue(replayStationSinkA.io.req_s1, 1), reqBuf.io.taskOut), reqArbTaskSinkA)
    }

    snpBuf.io.taskIn     <> rxsnp.io.task
    snpBuf.io.replay_s4  <> mainPipe.io.snpBufReplay_s4
    snpBuf.io.mshrStatus <> missHandler.io.mshrStatus

    sinkA.io.sliceId := io.sliceId

    reqArb.io.taskMSHR_s0              <> missHandler.io.tasks.mpTask
    reqArb.io.taskSinkA_s1             <> reqArbTaskSinkA
    reqArb.io.taskSnoop_s1             <> snpBuf.io.taskOut
    reqArb.io.taskSinkC_s1             <> sinkC.io.task
    reqArb.io.dirRead_s1               <> dir.io.dirRead_s1
    reqArb.io.resetFinish              <> dir.io.resetFinish
    reqArb.io.mpStatus_s45678          <> mainPipe.io.status
    reqArb.io.nonDataRespCnt           := sourceD.io.nonDataRespCntSinkC
    reqArb.io.mshrStatus               <> missHandler.io.mshrStatus
    reqArb.io.bufferStatus             := sourceD.io.bufferStatus
    reqArb.io.snpCnt                   := snpBuf.io.snpCnt
    reqArb.io.fromSinkC.willWriteDS_s1 := sinkC.io.toReqArb.willWriteDS_s1
    reqArb.io.fromSinkC.willWriteDS_s2 := sinkC.io.toReqArb.willWriteDS_s2

    mainPipe.io.reqDrop_s2_opt.foreach(_ := reqArb.io.reqDrop_s2_opt.getOrElse(false.B))
    mainPipe.io.mpReq_s2       <> reqArb.io.mpReq_s2
    mainPipe.io.dirResp_s3     <> dir.io.dirResp_s3
    mainPipe.io.replResp_s3    <> dir.io.replResp_s3
    mainPipe.io.mshrFreeOH_s3  := missHandler.io.mshrFreeOH_s3
    mainPipe.io.nonDataRespCnt := sourceD.io.nonDataRespCntMp
    mainPipe.io.txrspCnt       := txrsp.io.txrspCnt

    val cancelRefillWrite_s2 = mainPipe.io.retryTasks.stage2.fire && mainPipe.io.retryTasks.stage2.bits.isRetry_s2
    ds.io.dsWrite_s2                  <> sinkC.io.dsWrite_s2
    ds.io.refillWrite_s2.valid        := tempDS.io.toDS.refillWrite_s2.valid && !cancelRefillWrite_s2
    ds.io.refillWrite_s2.bits         := tempDS.io.toDS.refillWrite_s2.bits
    ds.io.fromMainPipe.dsRead_s3      <> mainPipe.io.toDS.dsRead_s3
    ds.io.fromMainPipe.dsWrWayOH_s3   <> mainPipe.io.toDS.dsWrWayOH_s3
    ds.io.fromMainPipe.mshrId_s3      := mainPipe.io.toDS.mshrId_s3
    ds.io.toTXDAT.dsResp_s7s8.ready   := txdat.io.data_s7s8.ready
    ds.io.toSourceD.dsResp_s7s8.ready := sourceD.io.data_s7s8.ready

    dir.io.dirWrite_s3 <> mainPipe.io.dirWrite_s3

    tempDS.io.fromDS.eccVec_s6        := ds.io.toTempDS.eccVec_s6
    tempDS.io.fromDS.write_s6         <> ds.io.toTempDS.write_s6
    tempDS.io.fromRXDAT.write         <> rxdat.io.toTempDS.write
    tempDS.io.fromSinkC.write         <> sinkC.io.toTempDS.write
    tempDS.io.fromReqArb.read_s1      <> reqArb.io.tempDsRead_s1
    tempDS.io.fromReqArb.dsWrSet_s1   := reqArb.io.dsWrSet_s1
    tempDS.io.fromReqArb.dsWrWayOH_s1 := reqArb.io.dsWrWayOH_s1

    sinkC.io.respMapCancel              <> missHandler.io.respMapCancel
    sinkC.io.respDest_s4                := mainPipe.io.allocDestSinkC_s4
    sinkC.io.fromReqArb.mayReadDS_s1    := reqArb.io.toSinkC.mayReadDS_s1
    sinkC.io.fromReqArb.willRefillDS_s1 := reqArb.io.toSinkC.willRefillDS_s1
    sinkC.io.fromReqArb.mayReadDS_s2    := reqArb.io.toSinkC.mayReadDS_s2
    sinkC.io.fromReqArb.willRefillDS_s2 := reqArb.io.toSinkC.willRefillDS_s2

    missHandler.io.mshrAlloc_s3       <> mainPipe.io.mshrAlloc_s3
    missHandler.io.resps.rxdat        <> rxdat.io.resp
    missHandler.io.resps.rxrsp        <> rxrsp.io.resp
    missHandler.io.resps.sinke        <> sinkE.io.resp
    missHandler.io.resps.sinkc        <> sinkC.io.resp
    missHandler.io.mshrStatus         <> dir.io.mshrStatus
    missHandler.io.replResp_s3        <> dir.io.replResp_s3
    missHandler.io.retryTasks         <> mainPipe.io.retryTasks
    missHandler.io.mshrEarlyNested_s2 <> mainPipe.io.mshrEarlyNested_s2
    missHandler.io.mshrNested_s3      <> mainPipe.io.mshrNested_s3
    missHandler.io.sliceId            := io.sliceId

    io.pCrdRetryInfoVec <> missHandler.io.pCrdRetryInfoVec

    txreq.io.mshrTask  <> missHandler.io.tasks.txreq
    txreq.io.mpTask_s3 := DontCare // TODO: connect to MainPipe or remove ?
    txreq.io.sliceId   := io.sliceId

    val cancelData_s2 = if (enableDataECC) {
        RegNext(reqArb.io.reqDrop_s2_opt.getOrElse(false.B), false.B)
    } else {
        reqArb.io.reqDrop_s2_opt.getOrElse(false.B)
    }
    sourceD.io.task_s2          <> mainPipe.io.sourceD_s2
    sourceD.io.data_s2          <> tempDS.io.toSourceD.data_s2
    sourceD.io.data_s2.valid    := tempDS.io.toSourceD.data_s2.valid && !cancelData_s2
    sourceD.io.task_s4          <> mainPipe.io.sourceD_s4
    sourceD.io.task_s7s8        <> mainPipe.io.sourceD_s7s8
    sourceD.io.data_s7s8.valid  := ds.io.toSourceD.dsResp_s7s8.valid
    sourceD.io.data_s7s8.bits   := ds.io.toSourceD.dsResp_s7s8.bits.data
    sourceD.io.grantMapWillFull := sinkE.io.grantMapWillFull

    txrsp.io.mshrTask  <> missHandler.io.tasks.txrsp
    txrsp.io.mpTask_s4 <> mainPipe.io.txrsp_s4

    if (enableBypassAtomic) {
        val amoDataBufResp = amoDataBufOpt.get.io.read.resp
        if (enableDataECC) {
            val amoDataBufRespValid = RegNext(amoDataBufResp.valid, false.B)
            txdat.io.data_s2.bits  := Mux(amoDataBufRespValid, RegEnable(amoDataBufResp.bits, amoDataBufResp.valid), tempDS.io.toTXDAT.data_s2.bits)
            txdat.io.data_s2.ready <> tempDS.io.toTXDAT.data_s2.ready
            txdat.io.data_s2.valid := amoDataBufRespValid || tempDS.io.toTXDAT.data_s2.valid && !cancelData_s2
        } else {
            txdat.io.data_s2.bits  := Mux(amoDataBufResp.valid, amoDataBufResp.bits, tempDS.io.toTXDAT.data_s2)
            txdat.io.data_s2.ready <> tempDS.io.toTXDAT.data_s2.ready
            txdat.io.data_s2.valid := amoDataBufResp.valid || tempDS.io.toTXDAT.data_s2.valid && !cancelData_s2
            assert(!(amoDataBufResp.valid && (tempDS.io.toTXDAT.data_s2.valid && !cancelData_s2)))
        }
    } else {
        txdat.io.data_s2       <> tempDS.io.toTXDAT.data_s2
        txdat.io.data_s2.valid := tempDS.io.toTXDAT.data_s2.valid && !cancelData_s2
    }
    txdat.io.task_s2         <> mainPipe.io.txdat_s2
    txdat.io.task_s7s8       <> mainPipe.io.txdat_s7s8
    txdat.io.data_s7s8.valid := ds.io.toTXDAT.dsResp_s7s8.valid
    txdat.io.data_s7s8.bits  := ds.io.toTXDAT.dsResp_s7s8.bits.data

    sinkE.io.allocGrantMap <> sourceD.io.allocGrantMap

    if (optParam.sourcebHasLatch) {
        sourceB.io.task <> Queue(missHandler.io.tasks.sourceb, 1)
    } else {
        sourceB.io.task <> missHandler.io.tasks.sourceb
    }
    sourceB.io.grantMapStatus  <> sinkE.io.grantMapStatus
    sourceB.io.mpStatus_s45678 <> mainPipe.io.status
    sourceB.io.bufferStatus    := sourceD.io.bufferStatus

    io.eccError := RegNext(ds.io.eccError, false.B) || RegNext(tempDS.io.eccError, false.B)

    if (enablePrefetch) {
        sinkA.io.prefetchReqOpt.get <> io.prefetchReqOpt.get
        io.prefetchRespOpt.get      <> sourceD.io.prefetchRespOpt.get
        io.prefetchTrainOpt.get     <> mainPipe.io.prefetchTrainOpt.get
    }

    io.tl.d      <> sourceD.io.d
    io.tl.b      <> sourceB.io.b
    io.chi.txreq <> txreq.io.out
    io.chi.txrsp <> txrsp.io.out
    io.chi.txdat <> txdat.io.out
    io.chi.rxdat <> rxdat.io.rxdat
    io.chi.rxrsp <> rxrsp.io.rxrsp
    io.chi.rxsnp <> rxsnp.io.rxsnp

    if (hasLowPowerInterface) {
        val lowPowerCtrl = Module(new LowPowerCtrl)
        val powerState   = lowPowerCtrl.io.powerState
        lowPowerCtrl.io.lowPower        <> io.lowPowerOpt.get
        lowPowerCtrl.io.mshrStatus      <> missHandler.io.mshrStatus
        lowPowerCtrl.io.mpStatus_s123   <> reqArb.io.status
        lowPowerCtrl.io.mpStatus_s45678 <> mainPipe.io.status

        val sramWakeupTimer  = RegInit((sramRetentionWakeupCycles).U(log2Up(sramRetentionWakeupCycles + 1).W)) // Wait for SRAM retention wakeup
        val sramWakeupFinish = sramWakeupTimer === sramRetentionWakeupCycles.U
        when(sramWakeupTimer < sramRetentionWakeupCycles.U && powerState =/= PowerState.RETENTION) {
            sramWakeupTimer := sramWakeupTimer + 1.U
        }.elsewhen(sramRetentionWakeupCycles.U >= sramWakeupTimer && (powerState === PowerState.RETENTION || powerState === PowerState.SHUTDOWN)) {
            sramWakeupTimer := 0.U
        }

        rxsnp.io.sramWakeupFinishOpt.get := sramWakeupFinish
        rxsnp.io.powerStateOpt.get       := powerState
        lowPowerCtrl.io.rxsnpValid       := io.chi.rxsnp.valid
        lowPowerCtrl.io.channelReqValid  := reqArb.io.channelReqValidOpt.get
        lowPowerCtrl.io.retentionWakeup  := powerState === PowerState.RETENTION && io.chi.rxsnp.valid
        lowPowerCtrl.io.sramWakeupFinish := sramWakeupFinish

        reqArb.io.lowPowerStateOpt.get   := powerState
        reqArb.io.lowPowerTaskOpt_s1.get <> lowPowerCtrl.io.toReqArb

        // SRAM retention signals
        ds.io.sramRetentionOpt.get     := powerState === PowerState.RETENTION
        dir.io.sramRetentionOpt.get    := powerState === PowerState.RETENTION
        tempDS.io.sramRetentionOpt.get := powerState === PowerState.RETENTION
    }

    if (enableBypassAtomic) {
        amoDataBufOpt.get.io.write    <> sinkA.io.amoDataBufWrOpt.get
        amoDataBufOpt.get.io.read.req <> reqArb.io.amoDataBufRdOpt.get
        amoDataBufOpt.get.io.free     <> mainPipe.io.amoDataBufFreeOpt_s2.get
    }

    dontTouch(io)
}

object Slice extends App {
    val CFG_CLIENT = sys.env.get("CFG_CLIENT").getOrElse("2")
    println(s"CFG_CLIENT = $CFG_CLIENT")

    xs.utils.Constantin.init(false)

    val config = new Config((_, _, _) => {
        // Fake EdgeInKey for prefetcher depends on it
        case EdgeInKey =>
            new TLEdgeIn(
                client = TLClientPortParameters(
                    Seq(
                        TLMasterParameters.v1(
                            name = "TLMaster",
                            supportsProbe = TransferSizes(64, 64)
                        )
                    )
                ),
                manager = TLManagerPortParameters(
                    managers = Seq(TLSlaveParameters.v1(address = AddressSet(0x00000000L, 0xffffffffffffL).subtract(AddressSet(0x0L, 0x7fffffffL)))),
                    beatBytes = 32,
                    requestKeys = Seq(TLNanhuBusKey, ReqSourceKey)
                ),
                params = Parameters.empty,
                sourceInfo = SourceInfo.materialize
            )
        case TLUserKey => TLUserParams(aliasBits = 2, vaddrBits = 48)
        case L2ParamKey =>
            L2Param(
                nrClients = CFG_CLIENT.toInt,
                supportDCT = true,
                // dataEccCode = "none",
                dataEccCode = "secded",
                optParam = L2OptimizationParam(
                    reqBufOutLatch = false,
                    rxsnpHasLatch = false,
                    sinkcHasLatch = false,
                    sourcebHasLatch = false,
                    rxrspHasLatch = false,
                    sinkaStallOnReqArb = true,
                    mshrStallOnReqArb = true,
                    latchTempDsToDs = true
                ),
                prefetchParams = Seq(SimpleL2.prefetch.BOPParameters(virtualTrain = true)),
                hasLowPowerInterface = true,
                lowPowerMSHRFreeThreshold = 50
            )
        case DebugOptionsKey => DebugOptions(EnablePerfDebug = false)
    })

    GenerateVerilog(args, () => new Slice()(config), name = "Slice", release = false, split = true)
}
