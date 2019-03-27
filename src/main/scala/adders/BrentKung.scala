package adders

import chisel3._
import chisel3.experimental._
import scala.reflect.ClassTag

class PGBundle extends Bundle {
  val p = Bool()
  val g = Bool()
}

@chiselName
class UIntBrentKungAdderType(opModule: Boolean = true) extends AdderType[UInt] {

  def prefix[T](x: Seq[T], op: (T, T) => T)(implicit m: ClassTag[T]): Seq[T] = {

    if (x.length == 1) {
      return x
    }

    val topLayerOdds = x.indices.collect {
      case i if i % 2 == 1 => op(x(i - 1), x(i))
    }

    val topLayerEvens = x.indices.collect {
      case i if i % 2 == 0 => x(i)
    }

    val midLayerOdds = prefix(topLayerOdds, op)

    x.indices.collect {
      case 0 => x.head
      case i if i % 2 == 0 => op(midLayerOdds(i / 2 - 1), x(i))
      case i => midLayerOdds((i - 1) / 2) // Odd
    }
  }

  def pgOp(right: PGBundle, left: PGBundle): PGBundle = { // returns (p, g)

    def pgOpImpl(right: PGBundle, left: PGBundle) = {
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

      io.out := pgOpImpl(io.in.right, io.in.left)
    }

    if (opModule) {
      val carryOp = Module(new CarryOp)

      carryOp.io.in.left := left
      carryOp.io.in.right := right

      carryOp.io.out
    } else {
      pgOpImpl(right, left)
    }
  }

  def propagateGenerate(x: UInt, y: UInt): Seq[PGBundle] = { // returns (p[], g[])
    // p and g should be of equal size
    assert(x.getWidth == y.getWidth)

    for ((xi, yi) <- x.asBools zip y.asBools)
      yield {
        val pgi = Wire(new PGBundle)
        pgi.p := xi ^ yi
        pgi.g := xi & yi
        pgi
      }

  }

  override def add(x: UInt, y: UInt, cin: Bool): UInt = {
    val pgIn = propagateGenerate(x, y)
    val pgOut = prefix(pgIn, pgOp) // p, g of intervals

    val c = Seq(cin) ++ (for (pgi <- pgOut) yield pgi.g | (cin & pgi.p))
    val s = (for ((ci, pgi) <- c zip pgIn) yield ci ^ pgi.p) :+ c.last

    VecInit(s).asUInt()
  }
}