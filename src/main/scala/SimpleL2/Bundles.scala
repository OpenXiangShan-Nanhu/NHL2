package SimpleL2.Bundles

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink._
import scala.collection.immutable.ListMap
import SimpleL2._
import SimpleL2.Configs._
import SimpleL2.chi._

class TaskBundle(implicit p: Parameters) extends L2Bundle {
    val isCHIOpcode = Bool()
    val opcode      = UInt(5.W)                                                              // TL Opcode ==> 3.W    CHI RXRSP Opcode ==> 5.W
    val param       = UInt(math.max(3, Resp.width).W)                                        // if isCHIOpcode is true, param is equals to the resp field in CHI
    val channel     = UInt(L2Channel.width.W)
    val set         = UInt(setBits.W)
    val tag         = UInt(tagBits.W)
    val source      = UInt(math.max(tlBundleParams.sourceBits, chiBundleParams.txnIdBits).W) // CHI RXRSP TxnID ==> 12.W, if isCHIOpcode is true, source is equals to the resp field in CHI
    val corrupt     = Bool()
    val sink        = UInt(math.max(tlBundleParams.sinkBits, mshrBits).W)                    // also the alias name for mshrId
    val wayOH       = UInt(ways.W)
    val retToSrc    = Bool()
    val vaddrOpt    = vaddrBitsOpt.map(width => UInt(width.W))
    val needHintOpt = if (enablePrefetch) Some(Bool()) else None
    val aliasOpt    = aliasBitsOpt.map(width => UInt(width.W))
    val isAliasTask = Bool()
    val isMshrTask  = Bool()

    val isReplayTask = Bool() // TODO: this signal is only for debugging, should be removed in the final version

    val readTempDs = Bool()
    val tempDsDest = UInt(DataDestination.width.W)

    val updateDir    = Bool()
    val newMetaEntry = new DirectoryMetaEntryNoTag

    val snpHitWriteBack = Bool() // for Snoop nested MSHR
    val snpGotDirty     = Bool() // for Snoop nested MSHR
    val snpHitReq       = Bool()
    val snpHitMshrId    = UInt(mshrBits.W)

    val getSnpNestedReq_opt = if (optParam.mshrStallOnReqArb) None else Some(Bool())

    val srcID = UInt(chiBundleParams.nodeIdBits.W)
    val tgtID = UInt(chiBundleParams.nodeIdBits.W)
    val dbID  = UInt(chiBundleParams.dbIdBits.W)

    val fwdState_opt = if (supportDCT) Some(UInt(3.W)) else None                          // Used for DCT
    val fwdNID_opt   = if (supportDCT) Some(UInt(chiBundleParams.nodeIdBits.W)) else None // Used for DCT
    val fwdTxnID_opt = if (supportDCT) Some(UInt(chiBundleParams.txnIdBits.W)) else None  // Used for DCT

    val isLowPowerTaskOpt = if (hasLowPowerInterface) Some(Bool()) else None

    def resp = param             // alias to opcode, if isCHIOpcode is true
    def txnID = source           // alias to source, if isCHIOpcode is true
    def chiOpcode = opcode       // alias to opcode, if isCHIOpcode is true
    def mshrId = sink            // alias to sink, if isMshrTask is true
    def isReplTask = isAliasTask // alias to isAliasTask, if isMshrTask is true

    def isSnoop = channel === L2Channel.ChannelB && !isMshrTask
    def isChannelA = channel.asUInt(0) && !isMshrTask
    def isChannelB = channel.asUInt(1) && !isMshrTask
    def isChannelC = channel.asUInt(2) && !isMshrTask
    def isTXREQ = channel === L2Channel.TXREQ && !isMshrTask
    def isTXRSP = channel === L2Channel.TXRSP && !isMshrTask
    def isTXDAT = channel === L2Channel.TXDAT && !isMshrTask
}

class MergeTaskBundle(implicit p: Parameters) extends L2Bundle {
    val mshrId = UInt(mshrBits.W)
    val task   = new TaskBundle
}

class PrefetchRespWithSource(sourceBits: Int)(implicit p: Parameters) extends SimpleL2.prefetch.PrefetchResp {
    val source = UInt(sourceBits.W)
}

class CreditIO[T <: Data](gen: T) extends Bundle {
    val crdv  = Input(Bool())
    val valid = Output(Bool())
    val bits  = Output(gen)
}

object CreditIO {
    def apply[T <: Data](gen: T): CreditIO[T] = new CreditIO(gen)
}

class TLRespBundle(params: TLBundleParameters)(implicit p: Parameters) extends L2Bundle {
    val opcode = UInt(3.W)
    val param  = UInt(3.W)
    val source = UInt(params.sourceBits.W)
    val sink   = UInt(params.sinkBits.W)
    val set    = UInt(setBits.W)
    val tag    = UInt(tagBits.W)
    val last   = Bool()
}

class LowPowerIO(implicit p: Parameters) extends L2Bundle {
    val req  = Flipped(ValidIO(UInt(2.W))) // 0: OFF, 1: RET, 2: ON
    val resp = ValidIO(Bool())             // 0: ERR, 1: OK
    val idle = Output(Bool())              // Whether the L2 is idle(no transactions are being processed)

    // Power modes:
    // Only allow 4 types of power mode transitions, which is:
    // (1) ON  -> OFF
    // (2) OFF -> ON
    // (3) ON  -> RET
    // (4) RET -> ON
    def OFF = 0.U(2.W)
    def RETENTION = 1.U(2.W)
    def ON = 2.U(2.W)

    def isRetention = req.valid && req.bits === RETENTION
    def isOn = req.valid && req.bits === ON
    def isOff = req.valid && req.bits === OFF
}

class LowPowerToReqArb(implicit p: Parameters) extends L2Bundle {
    val set    = UInt(setBits.W)
    val wayIdx = UInt(wayBits.W)
}
