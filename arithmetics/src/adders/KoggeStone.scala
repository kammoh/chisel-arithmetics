package adders

import chisel3._

import scala.annotation.tailrec

trait KoggeStone[T <: Data] extends PrefixAdder[T] with AdderGraph[T] { self: Module =>
  
  override def buildPrefixAdder(in: Seq[(Option[T], Option[T])]): Seq[(Option[T], Option[T])] = {
    println(s"KoggeStone of width ${in.length}")

    @tailrec
    def genLevels(in: Seq[(Option[T], Option[T])], level: Int = 1): Seq[(Option[T], Option[T])] = {
      if (level < in.length)
        genLevels(
          in.take(level) ++ in.zipWithIndex.zip(in.drop(level)).map { case ((left, j), right) =>
            mkCell(left, right, level, j + level, j)
          }, // { case (right, left) => blackCell(right, left) },
          2 * level
        )
      else
        in
    }

    val ret = genLevels(in)

    save(s"${desiredName}.drawio")
    ret
  }
}

class KSAdder(val width: Int, val withCin: Boolean = false) extends BitsAdderModule with KoggeStone[Bool] {
  def this(width: Int) = this(width, false)
}
