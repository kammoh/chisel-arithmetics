package masking
package adder

import chisel3.util.ShiftRegister
import chisel3.experimental.SourceInfo

import chest.masking.SharedBool

import adders.FullAdder

class RCAdder(val width: Int, val order: Int, gadget: String)
    extends BooleanMaskedAdderModule
    with adders.RippleCarry[SharedBool] {

  override def pipelined = true
  override def g = Gadget(gadget, pipelined)
  override def delay = (depth - 1) * g.andMinDelay + delayUnbalance

  type T = SharedBool

  def sr(x: T, n: Int): T = if (n == 0) x else ShiftRegister(x, n)
  def sr(x: Option[T], n: Int): Option[T] = x.map(sr(_, n))
  def sr(pg: (Option[T], Option[T]), n: Int): (Option[T], Option[T]) = (sr(pg._1, n), sr(pg._2, n))

  override def nextLayer(pg: Seq[(Option[T], Option[T])], level: Int): Seq[(Option[T], Option[T])] = {
    val d = (pg.length - 1) * g.andMinDelay + delayUnbalance
    pg.zipWithIndex.map { case (pgi, i) =>
      if (i == level) {
        val l = level - 1
        val pd = d - l + 1 - g.andMaxDelay
        println(s"level=$level l=$l d=$d pd=$pd")
        val (s, c) = mkCell(FullAdder, sr(pgi, l), pg(i - 1), level, i - 1, i - 2)
        (sr(s, pd), c)
      } else
        pgi
    }
  }

  def delayUnbalance = g.andMaxDelay - g.andMinDelay
  override def halfAdder(a: Option[T], b: Option[T])(implicit sourceInfo: SourceInfo): (Option[T], Option[T]) = {
    val c = and(sr(a, delayUnbalance), b) // unblacned AND gadget, first input needs to be delayed
    val s = xor(sr(a, g.andMinDelay), sr(b, g.andMinDelay))
    (s, c)
  }

  override def fullAdder(a: Option[T], b: Option[T], cin: Option[T])(implicit sourceInfo: SourceInfo)
    : (Option[T], Option[T]) = {
    val c = majority(a, b, cin)
    val s = xor(sr(a, g.andMinDelay), sr(b, g.andMinDelay), cin)
    (s, c)
  }
}
