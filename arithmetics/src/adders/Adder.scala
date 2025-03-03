package adders

import chisel3.experimental.SourceInfo

trait Adder[T] {

  def filler(x: Option[T])(implicit sourceInfo: SourceInfo): Option[T] = x

  def add(x: Seq[T], y: Seq[T], cin: Option[T] = None): Seq[T]

  def xor(a: T, b: T)(implicit sourceInfo: SourceInfo): T

  def xor(a: T, b: Option[T])(implicit sourceInfo: SourceInfo): T = {
    b.map(xor(a, _)).getOrElse(a)
  }

  // def xor(a: Option[T], b: T): T = {
  //   a.map(xor(b, _)).getOrElse(b)
  // }

  def xor(a: Option[T], b: Option[T])(implicit sourceInfo: SourceInfo): Option[T] = {
    a.map(xor(_, b)).orElse(b)
  }

  def xor(a: Option[T], b: Option[T], c: Option[T])(implicit sourceInfo: SourceInfo): Option[T] = xor(xor(a, b), c)

  def xor(a: T, b: T, c: T)(implicit sourceInfo: SourceInfo): T = xor(xor(a, b), c)

  def xor(a: T, b: T, c: Option[T])(implicit sourceInfo: SourceInfo): T = xor(xor(a, b), c)

  def xor(a: Seq[T], b: Seq[T]): Seq[T] = (a zip b).map { case (ai, bi) => xor(ai, bi) }

  def xorSeq(a: Seq[Option[T]], b: Seq[Option[T]]): Seq[Option[T]] = (a zip b).map { case (ai, bi) => xor(ai, bi) }

  def xor(a: Seq[T], b: Seq[T], c: Seq[T]): Seq[T] = (a zip b zip c).map { case ((ai, bi), ci) => xor(ai, bi, ci) }

  def and(a: T, b: T)(implicit sourceInfo: SourceInfo): T

  def and(a: Option[T], b: Option[T])(implicit sourceInfo: SourceInfo): Option[T] = {
    (a, b) match {
      case (Some(ai), Some(bi)) => Some(and(ai, bi))
      case _ => None
    }
  }

  def and(a: Seq[T], b: Seq[T]): Seq[T] = (a zip b).map { case (ai, bi) => and(ai, bi) }

  def and3(a: T, b: T, c: T)(implicit sourceInfo: SourceInfo): T = and(a, and(b, c))

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

  def not(a: T)(implicit sourceInfo: SourceInfo): T

  /** [Toffoli](https://en.wikipedia.org/wiki/Toffoli_gate) (CCNOT) gate
    *
    * Computes the 3rd (non-trivial) output: c ^ (a & b)
    *
    * @param a
    * @param b
    * @param c
    * @return
    *   c ^ (a & b)
    */
  def toffoli(a: T, b: T, c: T)(implicit sourceInfo: SourceInfo): T = xor(c, and(a, b))

  // TODO: final?
  // We should really try only overridding the (T,T,T)->T methods instead
  def toffoli(a: Option[T], b: Option[T], c: Option[T])(implicit sourceInfo: SourceInfo): Option[T] = {
    (a, b, c) match {
      case (Some(a), Some(b), Some(c)) =>
        Some(toffoli(a, b, c))
      case (Some(a), Some(b), None) =>
        Some(and(a, b))
      case (_, _, Some(c)) =>
        Some(c)
      case _ =>
        None
    }
  }

  def majority(a: Option[T], b: Option[T], c: Option[T])(implicit sourceInfo: SourceInfo): Option[T] =
    (a, b, c) match {
      case (None, _, _) =>
        and(b, c)
      case (_, None, _) =>
        and(a, c)
      case (_, _, None) =>
        and(a, b)
      case (Some(a), Some(b), Some(c)) =>
        Some(majority(a, b, c))
    }

  // NOTE: a ^ b, b ^ c are NOT independent!!! Cannot be used with e.g., DOM gadget!!!
  def majority(a: T, b: T, c: T)(implicit sourceInfo: SourceInfo): T = toffoli(xor(a, b), xor(b, c), b)

  def majority(a: T, b: T, c: Option[T])(implicit sourceInfo: SourceInfo): T =
    majority(Some(a), Some(b), c).getOrElse(zero)

  // (p & c) | g  <->
  def genG(p: Option[T], g: Option[T], c: Option[T])(implicit sourceInfo: SourceInfo): Option[T] =
    toffoli(p, c, g)
  // and3Xor(p, c, g.map(not), g)

  def blackCell(pg: (Option[T], Option[T]), pgr: (Option[T], Option[T]))(implicit sourceInfo: SourceInfo)
    : (Option[T], Option[T]) =
    blackCell(pg._1, pg._2, pgr._1, pgr._2)

  def blackCell(p: Option[T], g: Option[T], pr: Option[T], gr: Option[T])(implicit sourceInfo: SourceInfo)
    : (Option[T], Option[T]) =
    (and(p, pr), genG(p, g, gr))

  def halfAdder(a: Option[T], b: Option[T])(implicit sourceInfo: SourceInfo): (Option[T], Option[T]) = {
    val c = and(a, b)
    val s = xor(a, b)
    (s, c)
  }

  def pgSum(a: Option[T], b: Option[T]): (Option[T], Option[T]) = {
    (xor(a, b), None)
  }

  def fullAdder(a: Option[T], b: Option[T], cin: Option[T])(implicit sourceInfo: SourceInfo): (Option[T], Option[T]) = {
    val c = majority(a, b, cin)
    val s = xor(a, b, cin)
    (s, c)
  }

  def grayCell(p: Option[T], g: Option[T], c: Option[T])(implicit sourceInfo: SourceInfo): (Option[T], Option[T]) =
    (None, genG(p, g, c))

}
