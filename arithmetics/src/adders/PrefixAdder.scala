package adders

import chisel3._
import chisel3.experimental.skipPrefix
import chisel3.experimental.SourceInfo

trait PrefixAdderBase[T] extends Adder[T] {

  def buildPrefixAdder(firstLayer: Seq[(Option[T], Option[T])]): Seq[(Option[T], Option[T])]

  def lastLayer(pgIn: Seq[(Option[T], Option[T])], pgs: Seq[(Option[T], Option[T])]): Seq[T]

  def initLayer(x: Seq[T], y: Seq[T], cin: Option[T]): Seq[(Option[T], Option[T])]
}

trait PrefixAdder[T <: Data] extends AdderGraph[T] with PrefixAdderBase[T] { self: Module =>

  def filler(pg: (Option[T], Option[T]))(implicit sourceInfo: SourceInfo): (Option[T], Option[T]) =
    (filler(pg._1), filler(pg._2))

  def lastLayer(p0g0: Seq[(Option[T], Option[T])], lastPg: Seq[(Option[T], Option[T])]): Seq[T] = {
    val gk = lastPg.unzip._2

    val depth = currentDepth

    ((None +: gk.init).zip(p0g0.zipWithIndex).map { case (ci, (pgi, j)) =>
      mkCell(PGSum, pgi, ci, depth, j)._1
    } :+ gk.last).map(_.getOrElse(zero))

  }

  def initLayer(x: Seq[T], y: Seq[T], cin: Option[T]): Seq[(Option[T], Option[T])] = {
    println("initLayer --- ")
    (mkCell(FullAdder, (x.headOption, y.headOption), cin, 0, 0) +: (x.tail).zip(y.tail).zipWithIndex.map {
      case ((xi, yi), j) =>
        mkCell(HalfAdder, (Some(xi), Some(yi)), None, 0, j + 1)
    }).zipWithIndex.map { case ((p, g), i) =>
      (
        p.map(p => WireDefault(p.suggestName(f"p0_$i")).suggestName(f"p0_$i")),
        g.map(g => WireDefault(g.suggestName(f"g0_$i")).suggestName(f"g0_$i"))
      )
    }

  }

  override def mkCell[C <: CellType](
    ct: C,
    pg: (Option[T], Option[T]),
    pgr: (Option[T], Option[T]),
    i: Int,
    j: Int,
    jr: Int
  )(implicit sourceInfo: SourceInfo): (Option[T], Option[T]) = {
    val (pi, gi) = super.mkCell(ct, pg, pgr, i, j, jr)

    pi.foreach(_.suggestName(s"p${i}_${j}"))
    gi.foreach(_.suggestName(s"g${i}_${j}"))

    (
      pi.map(_.suggestName(s"p${i}_${j}")).map(WireDefault(_).suggestName(s"p${i}_${j}")),
      gi.map(_.suggestName(s"g${i}_${j}")).map(WireDefault(_).suggestName(s"g${i}_${j}"))
    )
  }

  override def add(x: Seq[T], y: Seq[T], cin: Option[T] = None): Seq[T] = {
    // x and y should be of equal size
    assert(x.length == y.length, s"widths must be the same! width of x: ${x.length}, width of y: ${y.length} ")
    val n = x.length
    setN(n + 1)

    val pg0 = { initLayer(x, y, cin) }.zipWithIndex.map { case ((p, g), i) =>
      (
        p.map(p => WireDefault(p.suggestName(f"p0_$i")).suggestName(f"p0_$i")),
        g.map(g => WireDefault(g.suggestName(f"g0_$i")).suggestName(f"g0_$i"))
      )
    }
    val sum = skipPrefix { lastLayer(pg0, buildPrefixAdder(pg0)) }.zipWithIndex.map { case (si, i) =>
      WireDefault(si.suggestName(f"sum$i")).suggestName(f"sum$i")
    }

    println(
      s"depth: ${currentDepth}  blackCells: ${numBlackCells}  grayCells: ${numGrayCells}  total: ${numBlackCells + numGrayCells}"
    )

    save(s"${desiredName}.drawio")
    sum
  }

}
