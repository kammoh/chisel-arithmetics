package adders

import chisel3._

trait PrefixAdderBase[T] extends Adder[T] {

  def buildPrefixAdder(firstLayer: Seq[(Option[T], Option[T])]): Seq[(Option[T], Option[T])]

  def lastLayer(pgIn: Seq[(Option[T], Option[T])], pgs: Seq[(Option[T], Option[T])]): Seq[T]

  def initLayer(x: Seq[T], y: Seq[T], cin: Option[T]): Seq[(Option[T], Option[T])]
}

trait PrefixAdder[T] extends AdderGraph[T] with PrefixAdderBase[T] { self: Module =>

  def lastLayer(pg0: Seq[(Option[T], Option[T])], lastPg: Seq[(Option[T], Option[T])]): Seq[T] = {
    val gk = lastPg.unzip._2

    val depth = currentDepth

    ((None +: gk.init).zip(pg0.zipWithIndex).map { case (ci, (pgi, j)) =>
      mkCell(PGSum, pgi, ci, depth, j)._1
    } :+ gk.last).map(_.getOrElse(zero))

  }

  def initLayer(x: Seq[T], y: Seq[T], cin: Option[T]): Seq[(Option[T], Option[T])] = {
    mkCell(FullAdder, (x.headOption, y.headOption), cin, 0, 0) +: (x.tail).zip(y.tail).zipWithIndex.map {
      case ((xi, yi), j) =>
        mkCell(HalfAdder, (Some(xi), Some(yi)), None, 0, j + 1)
    }

  }

  override def add(x: Seq[T], y: Seq[T], cin: Option[T] = None): Seq[T] = {
    // x and y should be of equal size
    assert(x.length == y.length, s"widths must be the same! width of x: ${x.length}, width of y: ${y.length} ")
    val n = x.length
    setN(n + 1)

    val pg0 = initLayer(x, y, cin)
    val sum = lastLayer(pg0, buildPrefixAdder(pg0))

    println(
      s"depth: ${currentDepth}  blackCells: ${numBlackCells}  grayCells: ${numGrayCells}  total: ${numBlackCells + numGrayCells}"
    )

    save(s"${desiredName}.drawio")
    sum
  }

}
