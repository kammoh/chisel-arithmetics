package adders

import chisel3._
import chisel3.util.simpleClassName

abstract class BitsAdderModule extends Module with BitsAdder {
  def withAsserts: Boolean = true
  def width: Int
  def withCin: Boolean

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

  // atModuleBodyEnd {
  io.sum :#= add(io.a, io.b, io.cin)

  if (withAsserts) {
    assert(io.sum === io.a +& io.b + io.cin.getOrElse(0.B))
  }

}
