package adders

import chisel3._
import chisel3.util._

trait SklanskyAdder[T] extends PrefixAdder[T] { self: Module =>

  override def buildPrefixAdder(pg: Seq[(Option[T], Option[T])]): Seq[(Option[T], Option[T])] = {
    val n = pg.length
    (0 until log2Ceil(n))
      .foldLeft(pg) { case (prevPgs, i) =>
        buildLayer(prevPgs, i)
      }
  }

  def buildLayer(pgs: Seq[(Option[T], Option[T])], i: Int): Seq[(Option[T], Option[T])] = {
    pgs.zipWithIndex.map { case (pg, j) =>
      val ll = j >> i
      if ((ll & 1) == 1) {
        val jj = (ll << i) - 1 // j - (j % l) - 1
        mkCell(if (ll == 1) GrayCellT else BlackCellT, pg, pgs(jj), i + 1, j, jj)
      } else {
        pg
      }
    }
  }
}

class SKAdder(val width: Int, val withCin: Boolean) extends BitsAdderModule with SklanskyAdder[Bool] {
  def this(width: Int) = this(width, false)

}
