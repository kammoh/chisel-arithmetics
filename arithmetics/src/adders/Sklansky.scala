package adders

import chisel3._
import chisel3.util._
import chisel3.experimental.skipPrefix

trait SklanskyAdder[T] extends Sklansky[T] with AdderGraph[T] { self: Module =>

  override def initLayer(a: Seq[T], b: Seq[T], c: Option[T]): (Seq[Option[T]], Seq[Option[T]]) = {
    val (p0, g0) = super[Sklansky].initLayer(a, b, c)
    for (j <- p0.indices) {
      addBox(InCell(0, j))
    }
    (p0, g0)
  }

  override def add(a: Seq[T], b: Seq[T], cin: Option[T]): Seq[T] = {
    val n = a.length
    require(n == b.length, "Inputs must have the same width")

    setN(n + 1)

    val (p0, gk) = sklansky(a, b, cin)
    val ret = skipPrefix { p0.head +: xorSeq(p0.tail, gk.init) :+ gk.last }

    println(
      s"depth: ${log2Ceil(n)}  blackCells: ${numBlackCells}  grayCells: ${numGrayCells}  total: ${numBlackCells + numGrayCells}"
    )

    for (j <- p0.indices) {
      addBox(OutCell(log2Ceil(n) + 1, j))
    }

    save(s"${desiredName}.drawio")

    ret.map(_.getOrElse(zero))
  }

}

trait Sklansky[T] extends AdderGraph[T] {

  /** @param a
    * @param b
    * @param c
    * @return
    *   (P, G)
    */
  def initLayer(a: Seq[T], b: Seq[T], c: Option[T]): (Seq[Option[T]], Seq[Option[T]]) = {
    (
      (xor(a, c.map({ xor(b.head, _) +: b.tail }).getOrElse(b))).map(Some(_)),
      (majority(a(0), b(0), c) +: and(a.tail, b.tail)).map(Some(_))
    )
  }

  def nextLayer(p: Seq[Option[T]], g: Seq[Option[T]], i: Int): (Seq[Option[T]], Seq[Option[T]]) =
    nextLayer(p.zip(g), i).unzip

  def nextLayer(pgs: Seq[(Option[T], Option[T])], i: Int): Seq[(Option[T], Option[T])] = {
    val l = 1 << i
    pgs.zipWithIndex.map { case ((pj, gj), j) =>
      if ((j / l) % 2 == 1) {
        val jj = j - (j % l) - 1
        val (pjj, gjj) = pgs(jj)
        mkCell((pj, gj), (gjj, pjj.flatMap(Option.when(j >= 2 * l)(_))), i + 1, j, jj)
      } else {
        (pj, gj)
      }
    }
  }

  def buildLayers(pgs: Seq[(Option[T], Option[T])], depth: Int): (Seq[Seq[(Option[T], Option[T])]]) = {
    (0 until depth)
      .foldLeft(Seq(pgs)) { case (prevPgs, i) =>
        prevPgs :+ nextLayer(prevPgs.last, i)
      }
  }

  def build(pgs: Seq[(Option[T], Option[T])], depth: Int): (Seq[(Option[T], Option[T])]) = {
    (0 until depth)
      .foldLeft(pgs) { case (prevPgs, i) =>
        nextLayer(prevPgs, i)
      }
  }

  def sklansky(a: Seq[T], b: Seq[T], cin: Option[T]): (Seq[Option[T]], Seq[Option[T]]) = {
    val n = a.length
    require(n == b.length, "Inputs must have the same width")
    val (p0, g0) = initLayer(a, b, cin)
    val (_, gk) = build(p0.zip(g0), log2Ceil(n)).unzip
    (p0, gk)
  }

}

class SKAdder(val width: Int, val withCin: Boolean) extends BitsAdderModule with SklanskyAdder[Bool] {
  def this(width: Int) = this(width, false)

}
