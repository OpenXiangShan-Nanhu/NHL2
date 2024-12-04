package SimpleL2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

class AtomicDataBufferWrite(implicit p: Parameters) extends L2Bundle {
    val valid = Input(Bool())
    val ready = Output(Bool())
    val bits = new Bundle {
        val data = Input(UInt(tlBundleParams.dataBits.W))
    }
    val idx = Output(UInt(log2Ceil(nrAtomicDataBuffer).W))

    def fire: Bool = valid && ready
}

class AtomicDataBufferReadReq(implicit p: Parameters) extends L2Bundle {
    val idx = UInt(log2Ceil(nrAtomicDataBuffer).W)
}

class AtomicDataBufferRead(implicit p: Parameters) extends L2Bundle {
    val req  = Flipped(Valid(new AtomicDataBufferReadReq))
    val resp = Valid(UInt(dataBits.W))
}

class AtomicDataBufferFree(implicit p: Parameters) extends L2Bundle {
    val idx = UInt(log2Ceil(nrAtomicDataBuffer).W)
}

class AtomicDataBuffer(implicit p: Parameters) extends L2Module {
    val io = IO(new Bundle {
        val write = new AtomicDataBufferWrite                            // From SinkA
        val read  = new AtomicDataBufferRead                             // From RequestArbiter
        val free  = Flipped(Valid(UInt(log2Ceil(nrAtomicDataBuffer).W))) //  From MainPipe
    })

    val dataBuffers = Reg(Vec(nrAtomicDataBuffer, UInt(tlBundleParams.dataBits.W)))
    val valids      = RegInit(VecInit(Seq.fill(nrAtomicDataBuffer)(false.B)))
    val freeVec     = ~(valids.asUInt)
    val freeIdx     = PriorityEncoder(freeVec)

    assert((nrBeat * tlBundleParams.dataBits) == dataBits)

    val wrIdx    = freeIdx
    val rdIdx    = io.read.req.bits.idx
    val rdIdxReg = RegEnable(io.read.req.bits.idx, io.read.req.fire)

    when(io.write.fire) {
        dataBuffers(wrIdx) := io.write.bits.data

        valids(wrIdx) := true.B
        assert(!valids(wrIdx), "write to an valid buffer!")
    }

    when(io.free.fire) {
        valids(rdIdx) := false.B
        assert(valids(rdIdx), "free an invalid buffer!")
    }

    when(io.read.req.fire && io.write.fire) {
        assert(wrIdx =/= rdIdx, "write and read to the same buffer!")
    }

    io.read.resp.valid := RegNext(io.read.req.fire, false.B)
    io.read.resp.bits  := dataBuffers(rdIdxReg).asUInt

    io.write.idx   := freeIdx
    io.write.ready := freeVec.orR
}
