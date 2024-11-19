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

/*
    rdy:  Suggest power controller that L2 is preferring to retain.(Only works for retention)
    req:  Actual retention request.
    ack:  L2 accepts to change its mode.
    nack: L2 refuses to change its mode.

    Dynamic power mode transition, power controller issue request according rdy of devices
    fsm:      ---A--|--T--|--R--|--T--|------A-----|-T-|------A------|-T-|---R----
    rdy:      ___/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺\_____________/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺\________/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺
    req:      _______/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺\______________/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺\_________/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺
    ack:      _____________/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺\_______________________________/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺
    nack:     _________________________________________/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺\_________________
    pwr_mode: -----On------|-----Ret-----|--------------On---------------|--Ret---

    Static power mode transition, power controller issue request according to software firmware
    fsm:      ---A--|--T--|--R--|--T--|------A-----|-T-|------A------|-T-|---R----
    req:      _______/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺\______________/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺\_________/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺
    ack:      _____________/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺\_______________________________/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺
    nack:     _________________________________________/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺\_________________
    pwr_mode: -----On------|-----Ret-----|--------------On---------------|--Ret---
 */

class LowPowerIO(implicit p: Parameters) extends L2Bundle {
    val shutdown = new Bundle {
        val req = Input(Bool())
        val ack = Output(Bool()) // set to HIGH when the low power operation is done
    }

    val retention = new Bundle {
        val req = Input(Bool())
        val ack = Output(Bool())
        val rdy = Output(Bool()) // set to HIGH when the L2Cache can enter retention mode
    }
}

class LowPowerToReqArb(implicit p: Parameters) extends L2Bundle {
    val set    = UInt(setBits.W)
    val wayIdx = UInt(wayBits.W)
}
