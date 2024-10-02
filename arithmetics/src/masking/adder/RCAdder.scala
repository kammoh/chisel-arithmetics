package masking
package adder

import chisel3._
import chisel3.util._

import chest.masking._

class RCAdder(val width: Int, val order: Int = 1)
    extends Module
    with MaskedAdder
    with adders.RippleCarry[SharedBool] {

  val io = IO(new Bundle {
    val a = Input(Shared(numShares, width))
    val b = Input(Shared(numShares, width))
    val sum = Output(Shared(numShares, width + 1))
  })

  io.sum :#= add(io.a, io.b)

  val delay = log2Ceil(width) + 1

  val verifDelay = Module(new VerifModule(delay))

  when(~reset.asBool & verifDelay.valid && ShiftRegister(randInValid, delay)) {
    // printf(p"verifDelay.valid\n")

    assert(io.sum.unshared() === io.a.unshared() + io.b.unshared())
  }

}
