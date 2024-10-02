package adders

import chisel3._
import chisel3.experimental.skipPrefix

trait BrentKung[T <: Data] extends PrefixAdder[T] { self: Module =>

  override def buildPrefixAdder(firstLayer: Seq[(Option[T], Option[T])]): Seq[(Option[T], Option[T])] = {
    val (ret, depths) = buildPrefixAdderRec(firstLayer.map((_, 0)), 0).unzip

    ret
  }

  def buildPrefixAdderRec(prev: Seq[((Option[T], Option[T]), Int)], level: Int): Seq[((Option[T], Option[T]), Int)] = {

    if (prev.length == 1) {
      return prev
    }
    val two2Layer = 1 << (level)
    val topLayerOdds = prev.indices.collect {
      case j if j % 2 == 1 =>
        val ((left, lDepth), (right, rDepth)) = (prev(j), prev(j - 1))
        val l = lDepth.max(rDepth) + 1
        (mkCell(left, (if (j == 1) None else right._1, right._2), l, two2Layer * (j + 1) - 1, two2Layer * j - 1), l)
    }

    val midLayerOdds = skipPrefix { buildPrefixAdderRec(topLayerOdds, level + 1) }

    prev.indices.collect {
      case 0 => prev.head
      case j if j % 2 == 0 =>
        val ((left, lDepth), (right, rDepth)) = (prev(j), midLayerOdds(j / 2 - 1))
        val l = lDepth.max(rDepth) + 1
        mkCell(
          left,
          (None, right._2),
          l,
          two2Layer * (j + 1) - 1, // <- actual `j`
          two2Layer * (j) - 1
        ) -> l
      case i => midLayerOdds((i - 1) / 2) // Odd
    }
  }
}

class BKAdder(val width: Int, val withCin: Boolean) extends BitsAdderModule with BrentKung[Bool] {
  def this(width: Int) = this(width, false)
}
