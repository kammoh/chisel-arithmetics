package adders

import chisel3._

import scala.annotation.tailrec

trait KoggeStone[T <: Data] extends PrefixAdder[T] {
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

class KSAdder(val width: Int, val withCin: Boolean = false) extends BitsPrefixAdderModule with KoggeStone[Bool] {}
