package adders

import chisel3._

import scala.annotation.tailrec

trait KoggeStone[T <: Data] extends PrefixAdder[T] with AdderGraph[T] { self: Module =>

  override def buildPrefixAdder(in: Seq[(Option[T], Option[T])]): Seq[(Option[T], Option[T])] = {
    println(s"KoggeStone of width ${in.length}")

    @tailrec
    def genLevels(in: Seq[(Option[T], Option[T])], level: Int = 1): Seq[(Option[T], Option[T])] = {
      val twoToLevel = 1 << (level - 1)
      if (twoToLevel < in.length)
        genLevels(
          in.take(twoToLevel) ++ in.drop(twoToLevel).zip(in).zipWithIndex.map { case ((left, right), j) =>
            if (j < twoToLevel) println(s"level:$level left=${j + twoToLevel} right=${j}")
            mkCell(if (j < twoToLevel) GrayCellT else BlackCellT, left, right, level, j + twoToLevel, j)
          },
          level + 1
        )
      else
        in
    }
    genLevels(in)
  }
}

class KSAdder(val width: Int, val withCin: Boolean = false) extends BitsAdderModule with KoggeStone[Bool] {
  def this(width: Int) = this(width, false)
}
