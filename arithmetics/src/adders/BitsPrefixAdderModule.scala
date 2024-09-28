package adders

import chisel3._
import chisel3.util.simpleClassName

abstract class BitsPrefixAdderModule extends Module with BitsPerfixAdder {
  def withAsserts: Boolean = true
  def width: Int
  def withCin: Boolean

  // def genG(p: Bool, g: Bool, c: Bool): Bool = {
  //   // (p & c) | g
  //   (p & c & ~g) ^ g
  // }

// // returns next (P, G)
//   override def blackCell(p: Bool, g: Bool, c: Bool, d: Bool): (Bool, Bool) = (p & d, genG(p, g, c))

// // returns next (P, G)
//   override def grayCell(p: Bool, g: Bool, c: Bool): (Bool, Bool) = (0.B, genG(p, g, c))

  lazy val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val cin = Option.when(withCin)(Input(Bool()))
    val sum = Output(UInt((width + 1).W))
  })

  override def desiredName: String = {
    val clzName = simpleClassName(this.getClass)
    clzName + (if (clzName.toLowerCase.endsWith("adder")) "" else "Adder") + width
  }

  atModuleBodyEnd {
    io.sum :#= add(io.a, io.b, io.cin)

    if (withAsserts) {
      assert(io.sum === io.a +& io.b + io.cin.getOrElse(0.B))
    }

  }
}
