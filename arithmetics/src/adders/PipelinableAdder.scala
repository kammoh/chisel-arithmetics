package adders

import chisel3._
import chisel3.util._

trait PipelinableAdder[T <: Data] extends Adder[T] {
  case class PGWithDelay(p: Option[T], pDelay: Int, g: Option[T], gDelay: Int) {
    def toPG: (Option[T], Option[T]) = (p, g)
    def delayed(delay: Int): PGWithDelay =
      PGWithDelay(p.map(ShiftRegister(_, delay)), pDelay + delay, g.map(ShiftRegister(_, delay)), gDelay + delay)
  }

  def buildLayer(pgs: Seq[PGWithDelay], layer: Int): Seq[PGWithDelay]
}

trait PipelinedSklanskyAdder[T <: Data] extends PrefixAdder[T] with PipelinableAdder[T] { self: Module =>

  override def buildPrefixAdder(pg: Seq[(Option[T], Option[T])]): Seq[(Option[T], Option[T])] = {
    val n = pg.length
    (0 until log2Ceil(n))
      .foldLeft(pg.map { case (p, g) => PGWithDelay(p, 0, g, 0) }) { case (prevPgs, i) =>
        buildLayer(prevPgs, i)
      }
      .map { case PGWithDelay(p, _, g, _) => (p, g) }
  }

  val p_gr_delay = 1
  val p_pr_delay = 1

  def getDelays(cellType: CellType, pDelay: Int, gDelay: Int, pRightDelay: Int, gRightDelay: Int): (Int, Int) = {
    val pOutDelay = pDelay.max(pRightDelay) + p_pr_delay
    val gOutDelay = (pDelay.max(gRightDelay) + p_gr_delay).max(gDelay)
    (pOutDelay, gOutDelay)
  }

  def mkCellWithDelay(
    cellType: CellType,
    pgWD: PGWithDelay,
    pgRightWD: PGWithDelay,
    depth: Int,
    offset: Int,
    rightOffset: Int): PGWithDelay = {

    val PGWithDelay(p, pDelay, g, gDelay) = pgWD
    val PGWithDelay(pRight, pRightDelay, gRight, gRightDelay) = pgRightWD

    val (p_, g_) = mkCell(cellType, (p, g), (pRight, gRight), depth, offset, rightOffset)
    val (pDelay_, gDelay_) = getDelays(cellType, pDelay, gDelay, pRightDelay, gRightDelay)
    PGWithDelay(p_, pDelay_, g_, gDelay_)
  }

  def buildLayer(pgs: Seq[PGWithDelay], layer: Int): Seq[PGWithDelay] = {
    pgs.zipWithIndex.map { case (x, j) =>
      val ll = j >> layer
      if ((ll & 1) == 1) {
        val jj = (ll << layer) - 1 // j - (j % l) - 1
        mkCellWithDelay(if (ll == 1) GrayCellT else BlackCellT, x, pgs(jj), layer + 1, j, jj)
      } else {
        x.delayed(p_gr_delay)
      }
    }
  }
}
