package adders

import chisel3._

trait PrefixAdderBase[T] extends Adder[T] {

  def buildPrefixAdder(firstLayer: Seq[(Option[T], Option[T])]): Seq[(Option[T], Option[T])]

  def lastLayer(pgIn: Seq[(Option[T], Option[T])], pgs: Seq[(Option[T], Option[T])], cin: Option[T]): Seq[T]

  def propagateGenerate(x: Seq[T], y: Seq[T]): Seq[(Option[T], Option[T])] = {
    // x and y should be of equal size
    assert(x.length == y.length, s"widths must be the same! width of x: ${x.length}, width of y: ${y.length} ")

    x.zip(y).map { case (xi, yi) =>
      (Some(xor(xi, yi)), Some(and(xi, yi)))
    }
  }

  override def add(x: Seq[T], y: Seq[T], cin: Option[T] = None): Seq[T] = {
    val pg0 = propagateGenerate(x, y)
    lastLayer(pg0, buildPrefixAdder(pg0), cin)
  }
}

trait PrefixAdder[T <: Data] extends AdderGraph[T] with PrefixAdderBase[T] { self: Module =>

  def lastLayer(pgIn: Seq[(Option[T], Option[T])], pgs: Seq[(Option[T], Option[T])], cin: Option[T]): Seq[T] = {

    val depth = currentDepth
    val gk = pgs.zipWithIndex.map { case (pg, j) =>
      mkCell(pg, (None, cin), depth, j, -1)
    }.unzip._2 //  pgi.g | (cin & pgi.p)

    val finalDepth = currentDepth

    for (j <- (0 until gk.length)) {
      val node = addBox(OutCell(finalDepth, j))
      node.setY(node.y - boxHeight / 2)
    }

    val sum =
      (cin +: gk.init).zip(pgIn).map { case (ci, pgi) => xor(ci, pgi._1.getOrElse(zero)) } :+ gk.last.getOrElse(zero)
    save(s"${desiredName}.drawio")
    sum
  }

  override def propagateGenerate(x: Seq[T], y: Seq[T]): Seq[(Option[T], Option[T])] = {
    val n = x.length
    setN(n + 1)

    for (j <- x.indices) {
      addBox(InCell(0, j))
    }
    super.propagateGenerate(x, y)
  }

}
