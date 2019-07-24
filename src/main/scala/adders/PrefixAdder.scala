package adders

import chisel3.experimental.chiselName
import chisel3._

abstract class PrefixAdder[T <: Bits with Num[T]](carryOpAsModule: Boolean) extends AdderType[T] {

  def prefix(x: Seq[PGBundle]): Seq[PGBundle]

  @chiselName
  class PGBundle extends Bundle {
    val p = Bool()
    val g = Bool()
  }

  @chiselName
  def carryOp(right: PGBundle, left: PGBundle): PGBundle = {

    def carryOpImpl(right: PGBundle, left: PGBundle) = {
      val o = Wire(new PGBundle)
      o.g := left.g | (right.g & left.p)
      o.p := left.p & right.p
      o
    }

    class CarryOp extends Module {
      val io = IO(
        new Bundle {
          val in = Input(new Bundle {
            val right = new PGBundle
            val left = new PGBundle
          })
          val out = Output(new PGBundle)
        }
      )

      io.out := carryOpImpl(io.in.right, io.in.left)
    }

    if (carryOpAsModule) {
      val carryOp = Module(new CarryOp)

      carryOp.io.in.left := left
      carryOp.io.in.right := right

      carryOp.io.out
    } else {
      carryOpImpl(right, left)
    }
  }

  @chiselName
  def propagateGenerate(x: T, y: T): Seq[PGBundle] = {
    // x and y should be of equal size
    assert(x.getWidth == y.getWidth, s"widths must be the same! width of x: ${x.getWidth}, width of y: ${y.getWidth} ")

    for ((xi, yi) <- x.asUInt.asBools zip y.asUInt.asBools)
      yield {
        val pgi = Wire(new PGBundle)
        pgi.p := xi ^ yi
        pgi.g := xi & yi
        pgi
      }
  }

  @chiselName
  override def add(x: T, y: T, cin: Bool): T = {
    val pgIn = propagateGenerate(x, y)
    val pgOut = prefix(pgIn) // (p, g) [0, i]  i <- (0..width)

    val c = Seq(cin) ++ (for (pgi <- pgOut) yield pgi.g | (cin & pgi.p))
    val s = VecInit((for ((ci, pgi) <- c zip pgIn) yield ci ^ pgi.p) :+ c.last)

    s.asUInt.asTypeOf(x.pad(math.max(x.getWidth,y.getWidth) + 1))
  }
}
