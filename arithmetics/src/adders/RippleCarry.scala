package adders

import chisel3._

trait RippleCarry[T <: Data] extends PrefixAdder[T] { self: Module =>
  override def initLayer(x: Seq[T], y: Seq[T], cin: Option[T]): Seq[(Option[T], Option[T])] = {
    (None, cin) +: x.zip(y).zipWithIndex.map { case ((xi, yi), j) =>
      mkCell(NullIn, Some(xi) -> Some(yi), None, 0, j)
    }
  }

  def nextLayer(pg: Seq[(Option[T], Option[T])], i: Int): Seq[(Option[T], Option[T])] = {
    pg.zipWithIndex.map { case (pgi, j) =>
      if (j == i)
        mkCell(FullAdder, pgi, pg(j - 1), i, j - 1, j - 2)
      else
        pgi
    }
  }
  override def buildPrefixAdder(firstLayer: Seq[(Option[T], Option[T])]): Seq[(Option[T], Option[T])] = {
    (1 to firstLayer.length)
      .foldLeft(firstLayer) { case (prevPgs, i) =>
        nextLayer(prevPgs, i)
      }
      .tail
  }

  def lastLayerOption(pg0: Seq[(Option[T], Option[T])], lastPg: Seq[(Option[T], Option[T])]): Seq[Option[T]] = {
    val depth = currentDepth
    ((lastPg.zipWithIndex).map { case (pg, j) =>
      mkCell(NullOut, pg, None, depth, j)._1
    } :+ lastPg.last._2)
  }

  override def lastLayer(pg0: Seq[(Option[T], Option[T])], lastPg: Seq[(Option[T], Option[T])]): Seq[T] = {
    lastLayerOption(pg0, lastPg).map(_.getOrElse(zero))
  }
}

class RCAdder(val width: Int, val withCin: Boolean) extends BitsAdderModule with RippleCarry[Bool] {
  def this(width: Int) = this(width, false)
}
