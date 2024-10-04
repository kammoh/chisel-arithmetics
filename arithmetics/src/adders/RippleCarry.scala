package adders

import chisel3._
import chisel3.util.simpleClassName

trait RippleCarry[T] extends PrefixAdder[T] { self: Module =>
  // override def add(x: Seq[T], y: Seq[T], cin: Option[T]): Seq[T] = {

  //   val (cout, sum) =
  //     x.zip(y).foldLeft((cin, Seq.empty[Option[T]])) { case ((c, sums), (a, b)) =>
  //       (majority(Some(a), Some(b), c), sums :+ xor(Some(a), Some(b), c))
  //     }

  //   (sum :+ cout).map(_.getOrElse(zero))
  // }

  override def initLayer(x: Seq[T], y: Seq[T], cin: Option[T]): Seq[(Option[T], Option[T])] = {
    // mkCell(FullAdder, (x.headOption, y.headOption), cin, 0, 0) +:

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

  def ll(pg0: Seq[(Option[T], Option[T])], lastPg: Seq[(Option[T], Option[T])]): Seq[Option[T]] = {
    val depth = currentDepth
    ((lastPg.zipWithIndex).map { case (pg, j) =>
      mkCell(NullOut, pg, None, depth, j)._1
    } :+ lastPg.last._2)
  }

  override def lastLayer(pg0: Seq[(Option[T], Option[T])], lastPg: Seq[(Option[T], Option[T])]): Seq[T] = {
    ll(pg0, lastPg).map(_.getOrElse(zero))
  }
}

class RCAdder(val width: Int, val withCin: Boolean) extends BitsAdderModule with RippleCarry[Bool] {
  def this(width: Int) = this(width, false)
}

class BasicAdder(width: Int) extends Module {
  lazy val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    // val cin = Option.when(withCin)(Input(Bool()))
    val sum = Output(UInt((width + 1).W))
  })

  override def desiredName: String = simpleClassName(this.getClass) + width

  io.sum :#= io.a +& io.b
}
