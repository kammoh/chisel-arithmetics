package modular

import chisel3._
import chisel3.util._


//object ConstMult()

class InvariantDivider(q: Int, wIn: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled( UInt(wIn.W)))
    val out = Decoupled(new Bundle{
      val rem = UInt(log2Ceil(q).W)
      val div = UInt((wIn - log2Ceil(q)).W)
    })
  })

  when(io.out.valid && io.out.ready){
    // invariants
    // TODO combinational for now, need scoreboarding for pipelined case
    assert(io.in.bits === io.out.bits.div * q.U + io.out.bits.rem)
    assert(io.out.bits.rem < q.U)
  }


  // TODO combinational for now
  io.in.ready := io.out.ready
  io.out.valid := io.in.valid

}
