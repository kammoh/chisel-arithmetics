package adders

import chisel3._
import chisel3.util._
import chisel3.core.BundleLitBinding
import chisel3.experimental.chiselName

class AdderInput(val w: Int) extends Bundle {

  val x = UInt(w.W)
  val y = UInt(w.W)
  val cin = Bool()

  def Lit(aVal: UInt, bVal: UInt, cinVal: Bool = 0.B) = {
    val clone = cloneType
    clone.selfBind(BundleLitBinding(Map(
      clone.x -> litArgOfBits(aVal),
      clone.y -> litArgOfBits(bVal),
      clone.cin -> litArgOfBits(cinVal)
    )))
    clone
  }
}


class AdderIO(val w: Int) extends Bundle {
  val in = Flipped(Decoupled(new AdderInput(w)))
  val out = Decoupled(UInt((w + 1).W))
}


@chiselName
class Adder(width: Int) extends Module {
  val io = IO(
    new AdderIO(width)
  )

  // an implicit AdderType needs to be in score
  //  implicit val adderType = new UIntCarryLookaheadAdderType(4)
  implicit val adderType = new UIntBrentKungAdderType(carryOpAsModule = false)


  // either use as explicit (when need the carry-in):
//  io.out.bits := adderType.add(io.in.bits.x, io.in.bits.y, io.in.bits.cin)
  // or as implicit (with carry-in tied to 0)
    io.out.bits := io.in.bits.x ++& io.in.bits.y ++& io.in.bits.cin

  io.out.valid :=  io.in.valid
  io.in.ready := io.out.ready
}
