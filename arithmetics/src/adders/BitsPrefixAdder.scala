package adders

import chisel3._

trait BitsPerfixAdder extends BitsAdder with PrefixAdder[Bool] {

  def prefix(x: Seq[PGBundle]): Seq[PGBundle] = {
    throw new NotImplementedError("prefix method not implemented")
    x
  }
}

trait BitsAdder extends Adder[Bool] {

  def gen = Bool()

  def add(a: UInt, b: UInt, cin: Option[Bool]): UInt = {
    require(a.widthKnown && b.widthKnown, "Inputs must have known widths")
    val n = a.getWidth
    require(n == b.getWidth, "Inputs must have the same width")

    VecInit(add(a.asBools, b.asBools, cin)).asUInt
  }

  def grayCellU(p: UInt, g: UInt, c: Bool): UInt = {
    VecInit(p.asBools.zip(g.asBools).map { case (pj, gj) => genG(pj, gj, c) }).asUInt
  }

  override def xor(a: Bool, b: Bool): Bool = a ^ b
  override def and(a: Bool, b: Bool): Bool = a & b
  override def and3(a: Bool, b: Bool, c: Bool): Bool = a & b & c
  override def not(a: Bool): Bool = ~a

}
