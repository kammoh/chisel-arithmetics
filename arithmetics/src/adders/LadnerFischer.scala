package adders

import chisel3._
import chisel3.util.log2Ceil

trait LadnerFischer[T <: Data] extends PrefixAdder[T] { self: Module =>

  override def buildPrefixAdder(pg: Seq[(Option[T], Option[T])]): Seq[(Option[T], Option[T])] = {
    val n = pg.length
    val d = log2Ceil(n)
    val ll = (0 until d)
      .foldLeft(pg) { case (prevPgs, i) =>
        nextLayer(prevPgs, i)
      }
    ll.take(1) ++ ll.drop(1).grouped(2).zipWithIndex.flatMap {
      case (Seq(pg_r, pg_l), i) =>
        Seq(pg_r, mkCell(GrayCellT, pg_l, pg_r, d + 1, 2 * i + 2, 2 * i + 1))
      case (s, _) => s
    }

  }

  def nextLayer(prev: Seq[(Option[T], Option[T])], level: Int): Seq[(Option[T], Option[T])] = {
    val skip = 1 << level
    prev.grouped(2 * skip).zipWithIndex.foldLeft(Seq.empty[(Option[T], Option[T])]) { case (acc, (grp, grpIndex)) =>
      acc ++ grp.take(skip) ++ (skip until grp.size).map { k =>
        val j = grpIndex * 2 * skip + k
        val jj = grpIndex * 2 * skip + skip - 1
        if ((k & 0x1) == 1)
          mkCell(if (grpIndex > 0) BlackCellT else GrayCellT, grp(k), grp(skip - 1), level + 1, j, jj)
        else grp(k)
      }
    }
  }
}

class LFAdder(val width: Int, val withCin: Boolean) extends BitsAdderModule with LadnerFischer[Bool] {
  def this(width: Int) = this(width, false)
}
