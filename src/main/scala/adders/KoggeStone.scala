package adders

import chisel3._
import chisel3.experimental.chiselName

import scala.annotation.tailrec

@chiselName
class KoggeStone[T <: Data with Num[T]](carryOpAsModule: Boolean = false) extends PrefixAdder[T](carryOpAsModule) {
  override def prefix(in: Seq[PGBundle]): Seq[PGBundle] = {
    println(s"KoggeStone of width ${in.length}")

    @tailrec
    def genLevels(in: Seq[PGBundle], level: Int = 1): Seq[PGBundle] = {
      if (level < in.length)
        genLevels(in.take(level) ++ in.zip(in.drop(level)).map((carryOp _).tupled), 2 * level)
      else
        in
    }

    genLevels(in)
  }
}