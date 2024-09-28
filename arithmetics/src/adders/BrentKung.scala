package adders

import chisel3._

trait BrentKung[T <: Data] extends PrefixAdder[T] {

  override def prefix(x: Seq[PGBundle]): Seq[PGBundle] = {

    if (x.length == 1) {
      return x
    }

    val topLayerOdds = x.indices.collect {
      case i if i % 2 == 1 => carryOp(x(i - 1), x(i))
    }

    val midLayerOdds = prefix(topLayerOdds)

    x.indices.collect {
      case 0 => x.head
      case i if i % 2 == 0 => carryOp(midLayerOdds(i / 2 - 1), x(i))
      case i => midLayerOdds((i - 1) / 2) // Odd
    }
  }

}

class BKAdder(val width: Int, val withCin: Boolean) extends BitsPrefixAdderModule with BrentKung[Bool] {
  def this(width: Int) = this(width, false)
}
