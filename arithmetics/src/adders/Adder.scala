package adders

trait Adder[T] {

  def add(x: Seq[T], y: Seq[T], cin: Option[T] = None): Seq[T]

  def xor(a: T, b: T): T

  def xor(a: T, b: Option[T]): T = {
    b.map(xor(a, _)).getOrElse(a)
  }

  def xor(a: Option[T], b: T): T = {
    a.map(xor(b, _)).getOrElse(b)
  }

  def xor(a: Option[T], b: Option[T]): Option[T] = {
    a.map(xor(_, b)).orElse(b)
  }

  def xor(a: T, b: T, c: T): T = xor(xor(a, b), c)
  def xor(a: Seq[T], b: Seq[T]): Seq[T] = (a zip b).map { case (ai, bi) => xor(ai, bi) }

  def xorSeq(a: Seq[Option[T]], b: Seq[Option[T]]): Seq[Option[T]] = (a zip b).map { case (ai, bi) => xor(ai, bi) }

  def xor(a: Seq[T], b: Seq[T], c: Seq[T]): Seq[T] = (a zip b zip c).map { case ((ai, bi), ci) => xor(ai, bi, ci) }

  def and(a: T, b: T): T

  def and(a: Option[T], b: Option[T]): Option[T] = {
    (a, b) match {
      case (Some(ai), Some(bi)) => Some(and(ai, bi))
      case _ => None
    }
  }

  def and(a: Seq[T], b: Seq[T]): Seq[T] = (a zip b).map { case (ai, bi) => and(ai, bi) }

  def and3(a: T, b: T, c: T): T = and(a, and(b, c))

  def and3(a: Seq[T], b: Seq[T], c: Seq[T]): Seq[T] = and(a, and(b, c))

  def and(a: Seq[T], b: Seq[T], c: Seq[T]): Seq[T] = (a zip b zip c).map { case ((ai, bi), ci) => and3(ai, bi, ci) }
  def zero: T

  def and3Xor(a: T, b: T, c: T, d: T): T = xor(and3(a, b, c), d)

  def and3Xor(a: Option[T], b: Option[T], c: Option[T], d: Option[T]): Option[T] = {
    (a, b, c, d) match {
      case (Some(ai), Some(bi), Some(ci), Some(di)) => Some(and3Xor(ai, bi, ci, di))
      case (_, _, _, Some(di)) => Some(di)
      case _ => None
    }
  }

  def not(a: T): T

  def toffoli(a: T, b: T, c: T): T = xor(and(a, b), c)

  def majority(a: T, b: T, c: T): T = toffoli(xor(a, b), xor(b, c), b)

  def majority(a: T, b: T, c: Option[T]): T = toffoli(xor(a, b), c.map(xor(b, _)).getOrElse(b), b)

  def genG(p: T, g: T, c: T): T = {
    // (p & c) | g
    and3Xor(p, c, not(g), g)
  }

  def genG(p: T, g: T, c: Option[T]): T = c.map(and3Xor(p, _, not(g), g)).getOrElse(g)

  def genG(p: Option[T], g: Option[T], c: Option[T]): Option[T] = and3Xor(p, c, g.map(not), g)

  // returns next (P, G)
  def blackCell(p: T, g: T, c: T, d: T): (T, T) = (and(p, d), genG(p, g, c))

  def blackCell(pg: (Option[T], Option[T]), pgr: (Option[T], Option[T])): (Option[T], Option[T]) =
    blackCell(pg._1, pg._2, pgr._1, pgr._2)

  def blackCell(p: Option[T], g: Option[T], pr: Option[T], gr: Option[T]): (Option[T], Option[T]) =
    (and(p, pr), genG(p, g, gr))

  def grayCell(p: Option[T], g: Option[T], c: Option[T]): (Option[T], Option[T]) = (None, genG(p, g, c))

}