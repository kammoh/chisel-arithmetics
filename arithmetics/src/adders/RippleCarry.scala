package adders

import chisel3._

trait RippleCarry[T] extends AdderGraph[T] {
  override def add(x: Seq[T], y: Seq[T], cin: Option[T]): Seq[T] = {

    val (cout, sum) =
      x.zip(y).foldLeft((cin.getOrElse(zero), Seq.empty[T])) { case ((c, sums), (a, b)) =>
        (majority(a, b, c), sums :+ xor(a, b, c))
      }

    sum :+ cout
  }

}

class RCAdder(val width: Int, val withCin: Boolean) extends BitsAdderModule with RippleCarry[Bool] {
  def this(width: Int) = this(width, false)
}
