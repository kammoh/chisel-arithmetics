package masking
package adder

import chisel3._
import chest.masking._

import adders.PrefixAdder
import chisel3.layers.Verification
import chisel3.experimental.noPrefix

abstract class BooleanMaskedAdderModule extends Module with MaskedAdder with PrefixAdder[SharedBool] {
  val io = IO(new Bundle {
    val a = Input(Shared(numShares, width))
    val b = Input(Shared(numShares, width))
    val sum = Output(Shared(numShares, width + 1))
  })

  io.sum :#= noPrefix { add(io.a, io.b) }

  // after the adder is built
  val depth = currentDepth

  def delay = (depth + 1) * 1 + 2

  layer.block(Verification) {
    val verifDelay = Module(new VerifModule(delay))

    assume(randInValid === 1.B)

    // when(verifDelay.count =/= 0.U) {
    assume(io.a === RegNext(io.a))
    assume(io.b === RegNext(io.b))
    // assume(!reset.asBool)
    // }.otherwise {}

    val sumUnshared = io.sum.unshared()
    val expected = io.a.unshared() +& io.b.unshared() // RegInit(0.U)

    when(verifDelay.valid) {

      assert(
        sumUnshared === expected,
        cf"Expected :$expected  Received: ${sumUnshared}"
      )

    }

  
  }
}
