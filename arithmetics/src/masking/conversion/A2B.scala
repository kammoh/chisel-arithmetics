package masking
package conversion
package a2b

import chisel3._
import chisel3.layers._
import chisel3.util._

import chest._
import chest.masking._
import chisel3.reflect.DataMirror

class GoubinA2B(W: Int) extends Module {
val io = IO(new Bundle {
    val x = Input(Vec(2, UInt(W.W)))
    val rand = Input(UInt(W.W))
    val out = Output(BooleanShared(2, UInt(W.W)))
  })

  io.out :#= A2B(io.x, io.rand)

  layer.block(Verification.Assert) {
    val p = Pipe(!reset.asBool, io.x.reduce(_ + _), 1)
    when(p.valid) {
      assert(io.out.unshared() === p.bits)
    }
  }
}
class A2B(order: Int = 1, W: Int) extends Module {
  val numShares = order + 1
  val io = IO(new Bundle {
    val x = Input(Vec(numShares, UInt(W.W)))
    val rand = Input(UInt(W.W))
    val out = Output(BooleanShared(numShares, UInt(W.W)))
  })

  io.out :#= A2B(io.x, io.rand)

  layer.block(Verification.Assert) {
    val p = Pipe(!reset.asBool, io.x.reduce(_ + _), 1)
    when(p.valid) {
      assert(io.out.unshared() === p.bits)
    }
  }
}

object A2B {

  /** Secure Carry Save Adder from the paper ... CHES 2024
    */
  def secCSA(x: BooleanShared, y: BooleanShared, cin: BooleanShared, rand: UInt): (BooleanShared, BooleanShared) = {
    val numShares = x.numShares
    require(numShares == y.numShares && numShares == cin.numShares)
    val a = x ^ y
    val s = cin ^ a
    val g = DOM(pipelined = true)
    val c = (x ^ g.and(a, x ^ cin, rand.asBools)) << 1
    (s, c)
  }


  
  def apply(x: Vec[UInt], rand: UInt): BooleanShared = {
    val numShares = x.length
    numShares match {
      case 2 =>
        BooleanShared.from(x) // FIXME to be implemented
      case 3 =>
        val y1 = BooleanShared.from(Seq(x(0), 0.U, 0.U))
        val y2 = BooleanShared.from(Seq(0.U, x(1), 0.U))
        val y3 = BooleanShared.from(Seq(0.U, 0.U, x(2)))
        val (s, c) = secCSA(y1, y2, y3, rand)
        BooleanShared.from(x) // FIXME to be implemented
      case _ =>
        BooleanShared.from(x) // FIXME to be implemented
    }
  }
}
