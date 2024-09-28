package adders

import chisel3._

trait RippleCarry[T <: Data] extends Adder[T] {
  override def add(x: Seq[T], y: Seq[T], cin: Option[T]): Seq[T] = {

    val (cout, sum) =
      x.zip(y).foldLeft((cin.getOrElse(zero), Seq.empty[T])) { case ((c, sums), (a, b)) =>
        (majority(a, b, c), sums :+ xor(a, b, c))
      }

    sum :+ cout
  }

}

class RippleCarryUInt(val width: Int, val withCin: Boolean) extends BitsPrefixAdderModule with RippleCarry[Bool] {

  def this(width: Int) = this(width, false)

  override def add(x: Seq[Bool], y: Seq[Bool], cin: Option[Bool]): Vec[Bool] = {

    val (cout, sum) =
      io.a.asBools.zip(io.b.asBools).foldLeft((io.cin.getOrElse(0.B), Seq.empty[Bool])) { case ((c, sums), (a, b)) =>
        (majority(a, b, c), sums :+ xor(a, b, c))
      }

    VecInit(sum :+ cout)
  }
}
