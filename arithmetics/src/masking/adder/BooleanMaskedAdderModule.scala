package masking
package adder

import chisel3._
import chest.masking._

import adders.PrefixAdder
import chisel3.layers.Verification
import chisel3.experimental.noPrefix
import chisel3.util.Pipe

abstract class BooleanMaskedAdderModule extends Module with MaskedAdder with PrefixAdder[SharedBool] {

  def withCarryIn: Boolean = false
  def pipelined: Boolean = true

  val io = IO(new Bundle {
    val a = Input(Shared(numShares, width.W))
    val b = Input(Shared(numShares, width.W))
    val cin = Option.when(withCarryIn)(Input(SharedBool(numShares)))
    val sum = Output(Shared(numShares, (width + 1).W))
  })

  io.sum :#= noPrefix { add(io.a, io.b, io.cin) }

  def depth = currentDepth

  def delay = (depth) * g.andMinDelay

  println(s"delay=${delay} pipelined=${pipelined}")

  layer.block(Verification) {
    if (!pipelined) {
      layer.block(Verification.Assume) {
        when(!reset.asBool && !RegNext(reset.asBool)) {
          assume(io.a === RegNext(io.a))
          assume(io.b === RegNext(io.b))
        }
      }
    }
    layer.block(Verification.Assert) {
      val p = Pipe(!reset.asBool, io.a.unmasked() +& io.b.unmasked() + io.cin.map(_.unmasked()).getOrElse(0.B), delay)
      when(!reset.asBool && p.valid) {
        assert(io.sum.unmasked() === p.bits)
      }
    }
  }
  // layer.block(Verification) {
  //   val verifDelay = Module(new VerifModule(delay))

  //   assume(randInValid === 1.B)

  //   // when(verifDelay.count =/= 0.U) {
  //   assume(io.a === RegNext(io.a))
  //   assume(io.b === RegNext(io.b))
  //   // assume(randomBits === RegNext(randomBits))
  //   // assume(!reset.asBool)
  //   // }.otherwise {}

  //   val sumUnmasked = io.sum.unmasked()
  //   val expected = io.a.unmasked() +& io.b.unmasked() // RegInit(0.U)

  //   when(verifDelay.valid) {

  //     assert(
  //       sumUnmasked === expected,
  //       cf"Expected :$expected  Received: ${sumUnmasked}"
  //     )

  //   }

  // }
}
