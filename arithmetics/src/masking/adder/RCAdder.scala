package masking
package adder

import chest.masking.SharedBool

import chisel3.util.ShiftRegister
import adders.FullAdder
import chisel3.experimental.SourceInfo
import chisel3.dontTouch

class RCAdder(val width: Int, val order: Int, gadget: String)
    extends BooleanMaskedAdderModule
    with adders.RippleCarry[SharedBool] {
  override def g = Gadget(gadget)
  override def delay = depth * g.andMinDelay + 1

  type T = SharedBool

  def sr(x: T, n: Int): T = if (n == 0) x else ShiftRegister(x, n)
  def sr(x: Option[T], n: Int): Option[T] = x.map(sr(_, n))
  def sr(pg: (Option[T], Option[T]), n: Int): (Option[T], Option[T]) = (sr(pg._1, n), sr(pg._2, n))

  override def nextLayer(pg: Seq[(Option[T], Option[T])], level: Int): Seq[(Option[T], Option[T])] = {
    pg.zipWithIndex.map { case (pgi, i) =>
      if (i == level) {
        val d = pg.length - 1
        val delay = (d - 1) * g.andMinDelay + 2
        val l = if (level > 1) level - 1 else 0
        val sumAddDelay = delay - l
        println(s"level: $level/$d l=$l sd=$sumAddDelay")

        val (s, c) = mkCell(FullAdder, sr(pgi, l), pg(i - 1), level, i - 1, i - 2)
        s.map(dontTouch(_).suggestName(s"s${level}"))
        c.map(dontTouch(_).suggestName(s"c${level}"))

        (sr(s, sumAddDelay), c)
      } else
        pgi
    }
  }
  override def halfAdder(a: Option[T], b: Option[T])(implicit sourceInfo: SourceInfo): (Option[T], Option[T]) = {
    val c = and(sr(a, g.andMaxDelay - g.andMinDelay), b)
    val s = xor(a, b)
    (s, c)
  }
}
