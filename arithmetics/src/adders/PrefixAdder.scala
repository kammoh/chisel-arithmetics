package adders

import chisel3._


trait PrefixAdder[T <: Data] extends Adder[T] {

  def prefix(x: Seq[PGBundle]): Seq[PGBundle]

  override def add(x: Seq[T], y: Seq[T], cin: Option[T] = None): Seq[T] = {
    val pgIn = propagateGenerate(x, y)
    val pgOut = prefix(pgIn)

    val c =
      cin.getOrElse(zero) +: pgOut.map(pgi => genG(pgi.p, pgi.g, cin)).toSeq //  pgi.g | (cin & pgi.p))

    c.zip(pgIn).map { case (ci, pgi) => xor(ci, pgi.p) } :+ c.last

  }
}

trait Adder[T <: Data] {

  def add(x: Seq[T], y: Seq[T], cin: Option[T] = None): Seq[T]

  def carryOpAsModule: Boolean = false

  def gen: T

  def xor(a: T, b: T): T
  def xor(a: T, b: T, c: T): T = xor(xor(a, b), c)
  def xor(a: Seq[T], b: Seq[T]): Seq[T] = (a zip b).map { case (ai, bi) => xor(ai, bi) }
  def xor(a: Seq[T], b: Seq[T], c: Seq[T]): Seq[T] = (a zip b zip c).map { case ((ai, bi), ci) => xor(ai, bi, ci) }
  def and(a: T, b: T): T
  def and(a: Seq[T], b: Seq[T]): Seq[T] = (a zip b).map { case (ai, bi) => and(ai, bi) }
  def and3(a: T, b: T, c: T): T
  def and(a: Seq[T], b: Seq[T], c: Seq[T]): Seq[T] = (a zip b zip c).map { case ((ai, bi), ci) => and3(ai, bi, ci) }
  def zero = 0.U.asTypeOf(gen)

  def and3Xor(a: T, b: T, c: T, d: T): T = xor(and3(a, b, c), d)
  def not(a: T): T

  def toffoli(a: T, b: T, c: T): T = xor(and(a, b), c)

  def majority(a: T, b: T, c: T): T = toffoli(xor(a, b), xor(b, c), b)

  def majority(a: T, b: T, c: Option[T]): T = toffoli(xor(a, b), c.map(xor(b, _)).getOrElse(b), b)

  def genG(p: T, g: T, c: T): T = {
    // (p & c) | g
    and3Xor(p, c, not(g), g)
  }

  def genG(p: T, g: T, c: Option[T]): T = c.map(and3Xor(p, _, not(g), g)).getOrElse(g)

  // returns next (P, G)
  def blackCell(p: T, g: T, c: T, d: T): (T, T) = (and(p, d), genG(p, g, c))

  // returns next (P, G)
  def grayCell(p: T, g: T, c: T): (T, T) = (zero, genG(p, g, c))

  class PGBundle extends Bundle {
    val p: T = gen.cloneType
    val g: T = gen.cloneType
  }

  def propagateGenerate(x: Seq[T], y: Seq[T]): Seq[PGBundle] = {
    // x and y should be of equal size
    assert(x.length == y.length, s"widths must be the same! width of x: ${x.length}, width of y: ${y.length} ")

    for ((xi, yi) <- x.zip(y))
      yield {
        val pgi = Wire(new PGBundle)
        pgi.p := xor(xi, yi)
        pgi.g := and(xi, yi)
        pgi
      }
  }

  def carryOp(right: PGBundle, left: PGBundle): PGBundle = {
    val o = Wire(new PGBundle)
    o.g := genG(left.p, left.g, right.g) // left.g | (right.g & left.p)
    o.p := and(left.p, right.p)
    o
  }
}
