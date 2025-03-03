package adders

import chisel3._
import chisel3.util.log2Ceil
import chisel3.experimental.SourceInfo

trait LadnerFischer[T <: Data] extends PrefixAdder[T] { self: Module =>

  def filler1(pg: (Option[T], Option[T]), depth: Int, column: Int, msg:String = "")(implicit sourceInfo: SourceInfo)
    : (Option[T], Option[T]) = {
    print(s"filler1 depth=$depth column=$column $msg\n")
    // (pg._1, pg._2)
    filler(pg)
  }

  override def buildPrefixAdder(pg: Seq[(Option[T], Option[T])]): Seq[(Option[T], Option[T])] = {
    val n = pg.length
    val d = log2Ceil(n)
    val ll = (0 until d)
      .foldLeft(pg) { case (prevPgs, i) =>
        nextLayer(prevPgs, i)
      }
    filler1(ll.head, d + 1, 0) +: ll.tail
    // ll.head +: ll.tail
      .grouped(2)
      .zipWithIndex
      .flatMap {
        case (Seq(pg_r, pg_l), i) =>
          Seq(filler1(pg_r, d + 1, 2 * i + 1, "xx"), mkCell(GrayCellT, pg_l, pg_r, d + 1, 2 * i + 2, 2 * i + 1))
        case (s, _) => s //.map(filler1(_, d + 1, 2 * i + 2))
      }
      .toSeq

  }

  def nextLayer(prev: Seq[(Option[T], Option[T])], level: Int): Seq[(Option[T], Option[T])] = {
    val skip = 1 << level
    prev.grouped(2 * skip).zipWithIndex.foldLeft(Seq.empty[(Option[T], Option[T])]) { case (acc, (grp, grpIndex)) =>
      acc ++ grp.take(skip).zipWithIndex.map { case (pg, i) =>
        filler1(pg, level + 1, i + acc.length)
      } ++ (skip until grp.size).map { k =>
        val j = grpIndex * 2 * skip + k
        val jj = grpIndex * 2 * skip + skip - 1
        if (k % 2 == 1)
          mkCell(if (grpIndex > 0) BlackCellT else GrayCellT, grp(k), grp(skip - 1), level + 1, j, jj)
        else filler1(grp(k), level + 1, j)
      }
    }
  }
}

class LFAdder(val width: Int, val withCin: Boolean) extends BitsAdderModule with LadnerFischer[Bool] {
  def this(width: Int) = this(width, false)
}
