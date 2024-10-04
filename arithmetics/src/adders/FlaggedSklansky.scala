package adders

import chisel3._
import chisel3.util._

class FlaggedSklanskyAdder(val width: Int, val withCin: Boolean = false)
    extends BitsAdderModule
    with FlaggedSklansky[Bool] {}

trait FlaggedSklansky[T] extends AdderGraph[T] {

  /** @param a
    * @param b
    * @return
    *   (P, G)
    */
  def init(a: Seq[T], b: Seq[T]): (Seq[Option[T]], Seq[Option[T]]) = (xor(a, b).map(Some(_)), and(a, b).map(Some(_)))

  def nextLayer(p: Seq[Option[T]], g: Seq[Option[T]], i: Int): (Seq[Option[T]], Seq[Option[T]]) = {

    val l = 1 << i
    p.zip(g)
      .zipWithIndex
      .map { case ((pj, gj), j) =>
        //  checks whether a component is needed, else propagates the signals to the next stage

        if ((j / l) % 2 == 1) { // operators every other 2**i column in stage i
          val jj = j - (j % l) - 1
          val (pjj, gjj) = (p(jj), g(jj))
          // Black cells only
          blackCell(pj, gj, gjj, pjj)
        } else { // No operator, propagate the P and G signals to the next stage
          (pj, gj)
        }
      }
      .unzip
  }

  override def add(a: Seq[T], b: Seq[T], cin: Option[T]): Seq[T] = {
    val n = a.length
    require(n == b.length, "Inputs must have the same width")
    val (p0, g0) = init(a, b)
    val (pk, gk) = (0 until log2Ceil(n))
      .foldLeft((p0, g0)) { case ((pi, gi), i) =>
        nextLayer(pi, gi, i)
      }
    val gBar = pk.zip(gk).map { case (pj, gj) => genG(pj, gj, cin) }

    xorSeq(cin +: gBar, p0).map(_.getOrElse(zero)) /// }.getOrElse(p0.head +: xor(gBar, p0.tail))
  }
}
