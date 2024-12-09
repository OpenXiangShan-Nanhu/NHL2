package SimpleL2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils.Code
import xs.utils.sram.SinglePortSramTemplate
import Utils.{GenerateVerilog, LeakChecker}
import SimpleL2.Configs._
import SimpleL2.Bundles._
import dataclass.data
import freechips.rocketchip.util.SeqToAugmentedSeq

class DSRead()(implicit p: Parameters) extends L2Bundle {
    val set   = UInt(setBits.W)
    val wayOH = UInt(ways.W)
    val dest  = UInt(DataDestination.width.W)
}

class DSWrite()(implicit p: Parameters) extends L2Bundle {
    val set   = UInt(setBits.W)
    val wayOH = UInt(ways.W)
    val data  = UInt(dataBits.W)
}

class DSResp()(implicit p: Parameters) extends L2Bundle {
    val data = UInt(dataBits.W)
}

class DataStorage()(implicit p: Parameters) extends L2Module {
    val io = IO(new Bundle {

        /** Write interface for [[SinkC]] */
        val dsWrite_s2 = Flipped(DecoupledIO(new DSWrite))

        /** Refilled data from [[TempDataStorage]] */
        val refillWrite_s2 = Flipped(ValidIO(new DSWrite))

        /** Read interface for [[MainPipe]] */
        val fromMainPipe = new Bundle {
            val dsRead_s3    = Flipped(ValidIO(new DSRead))
            val mshrId_s3    = Input(UInt(mshrBits.W))
            val dsWrWayOH_s3 = Flipped(ValidIO(UInt(ways.W))) // Write wayOH can only be determined when directory result is read back
        }

        val toTempDS = new Bundle {
            val eccVec_s6 = Output(Vec(blockBytes / eccProtectBytes, UInt(dataEccBits.W)))
            val write_s6  = ValidIO(new TempDataWrite)
        }

        /** 
         * The data being read will be passed into [[TXDAT]], where data will be further transfered into 
         * next level cache.
         */
        val toTXDAT = new Bundle {
            val dsResp_s7s8 = DecoupledIO(new DSResp)
        }

        val toSourceD = new Bundle {
            val dsResp_s7s8 = DecoupledIO(new DSResp)
        }

        /** 
         * This signal indicates that there is an uncorrectable ECC error. 
         * It is also passed into the top-level of [[Slice]] and connect to the L2 top-level interrupt signal after one cycle delay.
         */
        val eccError = Output(Bool())

        val sramRetentionOpt = if (hasLowPowerInterface) Some(Input(Bool())) else None
    })

    val ready_s8    = WireInit(false.B)
    val sramReady   = WireInit(false.B)
    val wayConflict = WireInit(false.B)

    println(s"[${this.getClass().toString()}] eccProtectBits: ${eccProtectBits} dataEccBits: ${dataEccBits} eccEnable: ${enableDataECC}")

    val groupBytes = 16                      // TODO: parameterize
    val group      = blockBytes / groupBytes // 64 / 16 = 4, each CacheLine is splited into 4 groups of data bytes
    val dataSRAMs_flat = if (optParam.useFlatDataSRAM) {
        println(s"[${this.getClass().toString()}] DataSRAM depth: ${sets * ways} DataSRAM banks: ${group} DataSRAM bank width: ${(groupBytes / 8) * dataWithECCBits}")

        /**
         *  Flat DataSRAM:
         *
         *  [8-bytes + 8-bits(ECC)] * 2 = 144-bits
         *                                   
         *     SRAM_0     SRAM_1    SRAM_2    SRAM_3
         * |-------------------------------------------|          
         * | 144-bits | 144-bits | 144-bits | 144-bits | set_0, way_0
         * |-------------------------------------------|
         * | 144-bits | 144-bits | 144-bits | 144-bits | set_0, way_1
         * |-------------------------------------------|
         *                       .
         *                       .
         *                       .
         * |-------------------------------------------|          
         * | 144-bits | 144-bits | 144-bits | 144-bits | set_N way_0
         * |-------------------------------------------|
         * | 144-bits | 144-bits | 144-bits | 144-bits | set_N, way_1
         * |-------------------------------------------|
         *                       .
         *                       .
         *                       .
         */
        Some(Seq.fill(group) {
            Module(
                new SinglePortSramTemplate(
                    gen = Vec(groupBytes / 8, UInt(dataWithECCBits.W)),
                    set = sets * ways,
                    way = 1,
                    setup = 1,
                    latency = 2,
                    powerCtl = hasLowPowerInterface
                )
            )
        })
    } else None
    val dataSRAMs = if (!optParam.useFlatDataSRAM) {
        println(s"[${this.getClass().toString()}] DataSRAM depth: ${sets} DataSRAM banks: ${group * ways} DataSRAM bank width: ${(groupBytes / 8) * dataWithECCBits}")

        /**
         * Each cacheline(64-byte) should be seperated into groups of 16-bytes(128-bits) beacuse SRAM provided by foundary is typically less than 144-bits wide.
         * And we also need extra 8-bits for every 8-bytes to store ECC bits. The final SRAM bits = [8-bytes + 8-bits(ECC)] * 2 = 144-bits.
         *                                   way_0                                                          way_1                        ...
         *        SRAM_0                                SRAM_1    SRAM_2    SRAM_3         SRAM_4      SRAM_5    SRAM_6     SRAM_7       ...
         * |-------------------------------------------------------------------------|  |-------------------------------------------|        
         * | [8-bytes + 8-bits(ECC)] * 2 = 144-bits | 144-bits | 144-bits | 144-bits |  | 144-bits | 144-bits | 144-bits | 144-bits |    ...
         * |-------------------------------------------------------------------------|  |-------------------------------------------| 
         *                                       .                                                   .  
         *                                       .                                                   .
         *                                       .                                                   .
         * 
         */
        Some(
            Seq.fill(ways) {
                Seq.fill(group) {
                    Module(
                        new SinglePortSramTemplate(
                            gen = Vec(groupBytes / 8, UInt(dataWithECCBits.W)),
                            set = sets,
                            way = 1,
                            setup = 1,
                            latency = 2,
                            powerCtl = hasLowPowerInterface
                        )
                    )
                }
            }
        )
    } else None

    println(s"[${this.getClass().toString()}] dataSRAMs entry bits: ${Vec(groupBytes / 8, UInt(dataWithECCBits.W)).getWidth}")

    val latchTempDsToDs = enableDataECC && optParam.latchTempDsToDs || !enableDataECC

    // -----------------------------------------------------------------------------------------
    // Stage 2 (SinkC release write)
    // -----------------------------------------------------------------------------------------
    val wen_sinkC_s2     = io.dsWrite_s2.fire
    val wrSet_sinkC_s2   = io.dsWrite_s2.bits.set
    val wrWayOH_sinkC_s2 = io.dsWrite_s2.bits.wayOH
    val wrData_sinkC_s2  = io.dsWrite_s2.bits.data

    val wen_refill_s2     = io.refillWrite_s2.valid
    val wrData_refill_s2  = io.refillWrite_s2.bits.data
    val wrSet_refill_s2   = io.refillWrite_s2.bits.set
    val wrWayOH_refill_s2 = io.refillWrite_s2.bits.wayOH

    val fire_s2 = if (latchTempDsToDs) wen_sinkC_s2 || wen_refill_s2 else wen_sinkC_s2

    _assert(!(RegNext(io.dsWrite_s2.fire, false.B) && io.dsWrite_s2.fire), "continuous write!")

    // -----------------------------------------------------------------------------------------
    // Stage 3 (mainpipe read)
    // -----------------------------------------------------------------------------------------
    val ren_s3       = io.fromMainPipe.dsRead_s3.valid
    val rdDest_s3    = io.fromMainPipe.dsRead_s3.bits.dest
    val rdWayOH_s3   = io.fromMainPipe.dsRead_s3.bits.wayOH
    val rdSet_s3     = io.fromMainPipe.dsRead_s3.bits.set
    val rdMshrIdx_s3 = io.fromMainPipe.mshrId_s3

    // @formatter:off
    val wen_s3           = if (latchTempDsToDs) { RegNext(fire_s2, false.B) } else { RegNext(fire_s2, false.B) || wen_refill_s2 } 
    val wen_sinkC_s3     = if (latchTempDsToDs) { wen_s3 && RegEnable(wen_sinkC_s2, false.B, fire_s2) } else { RegNext(fire_s2, false.B) && RegEnable(wen_sinkC_s2, false.B, fire_s2) } 
    val wrData_sinkC_s3  = RegEnable(wrData_sinkC_s2, wen_sinkC_s2)
    val wrSet_sinkC_s3   = RegEnable(wrSet_sinkC_s2, wen_sinkC_s2)
    val wrWayOH_sinkC_s3 = Mux(io.fromMainPipe.dsWrWayOH_s3.valid, io.fromMainPipe.dsWrWayOH_s3.bits, RegEnable(wrWayOH_sinkC_s2, wen_sinkC_s2))
    
    val wen_refill_s3     = if (latchTempDsToDs) { wen_s3 && RegEnable(wen_refill_s2, false.B, fire_s2) } else wen_refill_s2
    val wrData_refill_s3  = if (latchTempDsToDs) { RegEnable(wrData_refill_s2, wen_refill_s2) } else wrData_refill_s2
    val wrSet_refill_s3   = if (latchTempDsToDs) { RegEnable(wrSet_refill_s2, wen_refill_s2) } else wrSet_refill_s2
    val wrWayOH_refill_s3 = if (latchTempDsToDs) { RegEnable(wrWayOH_refill_s2, wen_refill_s2) } else wrWayOH_refill_s2
    // @formatter:on

    val wrSet_s3   = Mux(wen_refill_s3, wrSet_refill_s3, wrSet_sinkC_s3)
    val wrWayOH_s3 = Mux(wen_refill_s3, wrWayOH_refill_s3, wrWayOH_sinkC_s3)
    val wrData_s3  = Mux(wen_refill_s3, wrData_refill_s3, wrData_sinkC_s3)

    val fire_s3 = ren_s3 || wen_s3

    _assert(PopCount(Cat(wen_sinkC_s3, wen_refill_s3)) <= 1.U, "multiple write! wen_sinkC_s3:%d, wen_refill_s3:%d", wen_sinkC_s3, wen_refill_s3)
    assert(!(wen_s3 && ren_s3 && wayConflict), "read and write at the same time with wayConflict! wen_sinkC_s3:%d, wen_refill_s3:%d", wen_sinkC_s3, wen_refill_s3)

    io.dsWrite_s2.ready := !wen_s3 && !ren_s3

    // -----------------------------------------------------------------------------------------
    // Stage 4 (read setup)
    // -----------------------------------------------------------------------------------------
    val valid_s4 = RegNext(fire_s3, false.B)

    val ren_s4       = valid_s4 && RegEnable(ren_s3, false.B, fire_s3)
    val rdWayOH_s4   = RegEnable(rdWayOH_s3, ren_s3)
    val rdSet_s4     = RegEnable(rdSet_s3, ren_s3)
    val rdDest_s4    = RegEnable(rdDest_s3, ren_s3)
    val rdMshrIdx_s4 = RegEnable(rdMshrIdx_s3, ren_s3)

    val wen_s4     = valid_s4 && RegEnable(wen_s3, false.B, fire_s3)
    val wrWayOH_s4 = RegEnable(wrWayOH_s3, wen_s3)
    val wrSet_s4   = RegEnable(wrSet_s3, wen_s3)
    val wrData_s4  = RegEnable(wrData_s3, wen_s3)

    if (optParam.useFlatDataSRAM) {
        val wrIdx_s4 = Cat(OHToUInt(wrWayOH_s4), wrSet_s4)
        val rdIdx_s4 = Cat(OHToUInt(rdWayOH_s4), rdSet_s4)

        dataSRAMs_flat.get.zipWithIndex.foreach { case (sram, groupIdx) =>
            sram.io.req.valid      := wen_s4 || ren_s4
            sram.io.req.bits.write := wen_s4
            sram.io.req.bits.addr  := Mux(wen_s4, wrIdx_s4, rdIdx_s4)
            sram.io.req.bits.mask.foreach(_ := 1.U)
            (0 until (groupBytes / eccProtectBytes)).foreach { i =>
                val wrGroupDatas = wrData_s4(groupBytes * 8 * (groupIdx + 1) - 1, groupBytes * 8 * groupIdx)
                sram.io.req.bits.data(0)(i) := dataCode.encode(wrGroupDatas(eccProtectBytes * 8 * (i + 1) - 1, eccProtectBytes * 8 * i))
            }

            if (hasLowPowerInterface) {
                sram.io.pwctl.get.ret  := io.sramRetentionOpt.get
                sram.io.pwctl.get.stop := false.B
            }

            _assert(!(sram.io.req.valid && !sram.io.req.ready), "dataSRAM request not ready!")
        }
    } else {
        dataSRAMs.get.zipWithIndex.foreach { case (srams, wayIdx) =>
            val wrWayEn = wrWayOH_s4(wayIdx)
            val rdWayEn = rdWayOH_s4(wayIdx)

            srams.zipWithIndex.foreach { case (sram, groupIdx) =>
                sram.io.req.valid      := wen_s4 && wrWayEn || ren_s4 && rdWayEn
                sram.io.req.bits.write := wen_s4
                sram.io.req.bits.addr  := Mux(wen_s4, wrSet_s4, rdSet_s4)
                sram.io.req.bits.mask.foreach(_ := 1.U)
                (0 until (groupBytes / eccProtectBytes)).foreach { i =>
                    val wrGroupDatas = wrData_s4(groupBytes * 8 * (groupIdx + 1) - 1, groupBytes * 8 * groupIdx)
                    sram.io.req.bits.data(0)(i) := dataCode.encode(wrGroupDatas(eccProtectBytes * 8 * (i + 1) - 1, eccProtectBytes * 8 * i))
                }

                if (hasLowPowerInterface) {
                    sram.io.pwctl.get.ret  := io.sramRetentionOpt.get
                    sram.io.pwctl.get.stop := false.B
                }

                _assert(!(sram.io.req.valid && !sram.io.req.ready), "dataSRAM request not ready!")
            }
        }
    }

    // -----------------------------------------------------------------------------------------
    // Stage 5 (read accept)
    // -----------------------------------------------------------------------------------------
    val ren_s5       = RegNext(ren_s4, false.B)
    val rdWayOH_s5   = RegEnable(rdWayOH_s4, ren_s4)
    val rdData_s5    = WireInit(0.U(dataBits.W))
    val rdDest_s5    = RegEnable(rdDest_s4, ren_s4)
    val rdMshrIdx_s5 = RegEnable(rdMshrIdx_s4, ren_s4)

    val wen_s5     = RegNext(wen_s4, false.B)
    val wrWayOH_s5 = RegEnable(wrWayOH_s4, wen_s4)

    val hasAccess_s4   = RegNext(wen_s3 || ren_s3, false.B)
    val hasAccess_s5   = RegNext(wen_s4 || ren_s4, false.B)
    val accessWayOH_s4 = Mux(wen_s4, RegNext(wrWayOH_s3), RegNext(rdWayOH_s3))
    val accessWayOH_s5 = Mux(wen_s5, RegNext(wrWayOH_s4), RegNext(rdWayOH_s4))
    wayConflict := hasAccess_s4 && hasAccess_s5 && (accessWayOH_s4 & accessWayOH_s5).orR
    sramReady   := !wayConflict

    /**
     * It is permitted that [[DataStorage]] can be access by different wayOH during the consective cycles.
     * However, it is not permitted that [[DataStorage]] is accessed by the same wayOH during the consective cycle.
     */
    val wen_sinkC_s4  = RegNext(wen_sinkC_s3, false.B)
    val wen_refill_s4 = RegNext(wen_refill_s3, false.B)
    val wen_sinkC_s5  = RegNext(wen_sinkC_s4, false.B)
    val wen_refill_s5 = RegNext(wen_refill_s4, false.B)
    assert(
        !((wen_s5 || ren_s5) && !sramReady),
        "sram is not ready! wen_s4:%d(wen_sinkC_s4:%d wen_refill_s4:%d), ren_s4:%d, wen_s5:%d(wen_sinkC_s5:%d wen_refill_s5:%d), ren_s5:%d",
        wen_s4,
        wen_sinkC_s4,
        wen_refill_s4,
        ren_s4,
        wen_s5,
        wen_sinkC_s5,
        wen_refill_s5,
        ren_s5
    )

    // -----------------------------------------------------------------------------------------
    // Stage 6 (read finish)
    // -----------------------------------------------------------------------------------------
    val ren_s6          = RegNext(ren_s5, false.B)
    val rdWayOH_s6      = RegEnable(rdWayOH_s5, ren_s5)
    val rdData_s6       = WireInit(0.U(dataBits.W))
    val rdDest_s6       = RegEnable(rdDest_s5, ren_s5)
    val rdMshrIdx_s6    = RegEnable(rdMshrIdx_s5, ren_s5)
    val fire_s6         = ren_s6 && rdDest_s6 =/= DataDestination.TempDataStorage
    val rdDataRawVec_s6 = if (optParam.useFlatDataSRAM) None else Some(VecInit(dataSRAMs.get.zipWithIndex.map { case (srams, wayIdx) => VecInit(srams.map(_.io.resp.bits.data(0))) }))
    val rdDataRaw_s6    = if (optParam.useFlatDataSRAM) VecInit(dataSRAMs_flat.get.map(_.io.resp.bits.data(0))) else Mux1H(rdWayOH_s6, rdDataRawVec_s6.get)
    val rdDataVec_s6 = if (optParam.useFlatDataSRAM) {
        VecInit(rdDataRaw_s6.map { dataVec =>
            VecInit(dataVec.map { d =>
                dataCode.decode(d).uncorrected
            }).asUInt
        })
    } else {
        VecInit(rdDataRaw_s6.map { dataVec =>
            VecInit(dataVec.map { d =>
                dataCode.decode(d).uncorrected
            }).asUInt
        })
    }

    if (!optParam.useFlatDataSRAM) {
        require(rdDataRawVec_s6.get.head.length == group)
        require(rdDataRawVec_s6.get.length == ways)
    }
    require(rdDataRaw_s6.length == group)
    require(rdDataVec_s6.length == group)

    rdData_s6                      := rdDataVec_s6.asUInt
    io.toTempDS.write_s6.valid     := rdDest_s6 === DataDestination.TempDataStorage && ren_s6
    io.toTempDS.write_s6.bits.data := rdData_s6 // rdData without ECC bits, ECC check is located in TempDataStorage, so we don't need to add ECC bits here
    io.toTempDS.write_s6.bits.mask := Fill(nrBeat, 1.U(1.W))
    io.toTempDS.write_s6.bits.idx  := rdMshrIdx_s6
    if (enableDataECC) {
        io.toTempDS.eccVec_s6 := VecInit(
            rdDataRaw_s6.map { dataVec =>
                dataVec.asTypeOf(Vec(groupBytes / 8, UInt(dataWithECCBits.W))).map(d => d.head(dataEccBits)).asUInt
            }
        ).asUInt.asTypeOf(Vec(blockBytes / eccProtectBytes, UInt(dataEccBits.W)))
    } else {
        io.toTempDS.eccVec_s6 := DontCare
    }

    // -----------------------------------------------------------------------------------------
    // Stage 7 (data output) / (ECC check)
    // -----------------------------------------------------------------------------------------
    val ren_s8_dup           = WireInit(false.B)
    val readToSourceD_s8_dup = WireInit(false.B)
    val readToTXDAT_s8_dup   = WireInit(false.B)

    val ren_s7           = RegInit(false.B)
    val rdData_s7        = WireInit(0.U(dataBits.W))
    val rdDataRaw_s7     = RegEnable(rdDataRaw_s6, fire_s6)
    val rdDest_s7        = RegEnable(rdDest_s6, fire_s6)
    val readToSourceD_s7 = rdDest_s7 === DataDestination.SourceD
    val readToTXDAT_s7   = rdDest_s7 === DataDestination.TXDAT
    val fire_s7          = ren_s7 && ready_s8 && (io.toSourceD.dsResp_s7s8.valid && !io.toSourceD.dsResp_s7s8.ready || io.toTXDAT.dsResp_s7s8.valid && !io.toTXDAT.dsResp_s7s8.ready)

    val rdDataVecDecoded_s7       = rdDataRaw_s7.map(dataVec => dataVec.map(d => dataCode.decode(d)))
    val rdDataVec_s7              = VecInit(rdDataVecDecoded_s7.map(decodedDataVec => VecInit(decodedDataVec.map(decodedData => Mux(decodedData.correctable, decodedData.corrected, decodedData.uncorrected))).asUInt))
    val rdDataHasErr_s7           = VecInit(rdDataVecDecoded_s7.map(decodedDataVec => VecInit(decodedDataVec.map(decodedData => decodedData.error)).asUInt)).asUInt.orR
    val rdDataHasUncorrectable_s7 = VecInit(rdDataVecDecoded_s7.map(decodedDataVec => VecInit(decodedDataVec.map(decodedData => decodedData.uncorrectable)).asUInt)).asUInt.orR

    rdData_s7 := rdDataVec_s7.asUInt

    // TODO: ECC info should be saved in some kind of CSR registers. For now, just ignore it.
    if (enableDataECC) {
        assert(
            !(ren_s7 && rdDataHasErr_s7 && rdDataHasUncorrectable_s7),
            "TODO: Data has error rdDataVec_s7:%x, rdDataHasErr_s7:%d, rdDataHasUncorrectable_s7:%d",
            rdDataVec_s7.asUInt,
            rdDataHasErr_s7,
            rdDataHasUncorrectable_s7
        )

        io.eccError := ren_s7 && rdDataHasUncorrectable_s7
    } else {
        io.eccError := false.B
    }

    when(fire_s6) {
        ren_s7 := true.B
    }.elsewhen((io.toSourceD.dsResp_s7s8.fire && readToSourceD_s7 && !(ren_s8_dup && readToSourceD_s8_dup) || io.toTXDAT.dsResp_s7s8.fire && readToTXDAT_s7 && !(ren_s8_dup && readToTXDAT_s8_dup)) && !fire_s6 && ren_s7) {
        ren_s7 := false.B
    }.elsewhen(fire_s7 && !fire_s6) {
        ren_s7 := false.B
    }

    assert(!(fire_s6 && ren_s7), "stage 7 is full!")
    assert(!(ren_s7 && readToSourceD_s7 && readToTXDAT_s7))
    LeakChecker(ren_s7, !ren_s7, Some("ren_s7"), maxCount = deadlockThreshold)

    // -----------------------------------------------------------------------------------------
    // Stage 8 (data output)
    // -----------------------------------------------------------------------------------------
    val ren_s8           = RegInit(false.B)
    val rdData_s8        = RegEnable(rdData_s7, fire_s7)
    val rdDest_s8        = RegEnable(rdDest_s7, fire_s7)
    val readToTXDAT_s8   = rdDest_s8 === DataDestination.TXDAT
    val readToSourceD_s8 = rdDest_s8 === DataDestination.SourceD
    val fire_s8          = io.toTXDAT.dsResp_s7s8.fire && readToTXDAT_s8 || io.toSourceD.dsResp_s7s8.fire && readToSourceD_s8
    ren_s8_dup           := ren_s8
    readToSourceD_s8_dup := readToSourceD_s8
    readToTXDAT_s8_dup   := readToTXDAT_s8
    ready_s8             := !ren_s8

    when(fire_s7) {
        ren_s8 := true.B
    }.elsewhen(fire_s8 && ren_s8 && !fire_s7) {
        ren_s8 := false.B
    }

    assert(!(ren_s8 && readToSourceD_s8 && readToTXDAT_s8))
    LeakChecker(ren_s8, !ren_s8, Some("ren_s8"), maxCount = deadlockThreshold)

    io.toTXDAT.dsResp_s7s8.valid     := ren_s7 && readToTXDAT_s7 || ren_s8 && readToTXDAT_s8
    io.toTXDAT.dsResp_s7s8.bits.data := Mux(readToTXDAT_s8 && ren_s8, rdData_s8, rdData_s7)

    io.toSourceD.dsResp_s7s8.valid     := ren_s7 && readToSourceD_s7 || ren_s8 && readToSourceD_s8
    io.toSourceD.dsResp_s7s8.bits.data := Mux(readToSourceD_s8 && ren_s8, rdData_s8, rdData_s7)

    dontTouch(io)
}

object DataStorage extends App {
    val config = SimpleL2.DefaultConfig()

    GenerateVerilog(args, () => new DataStorage()(config), name = "DataStorage", split = true)
}
