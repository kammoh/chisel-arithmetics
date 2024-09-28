package masking

import chisel3._
import chisel3.util._
import chisel3.experimental.skipPrefix

import chest.markDontTouch
import chest.crossProduct

object DOM {
  def numRandBits(n: Int, t: Int): Int = (BigInt(t + 1).pow(n - 1).toInt - 1) * t

  def and(in: Seq[Vec[UInt]], rand: Vec[UInt], valid: Bool): Vec[UInt] = {
    val n = in.length
    val numShares = in.head.length
    require(in.forall(_.length == numShares), "all inputs must have the same number of shares")

    val pipelined = true

    def optReg(u: UInt): UInt = if (pipelined) RegEnable(u, valid) else u

    val ppMap = skipPrefix {
      crossProduct(in.map(_.zipWithIndex)).groupMapReduce(_.head._2) { case zz =>
        val (inputs, shareIds) = zz.unzip
        val p = inputs.reduce(_ & _).suggestName(s"p_${shareIds.mkString("_")}")
        val ri = shareIds.foldLeft(0)((acc, j) => acc * numShares + (j + numShares - shareIds.head) % numShares)
        val pp =
          if (shareIds.distinct.length == 1) optReg(p)
          else {
            val r =
              if (shareIds.head == numShares - 1) rand(ri - 1)
              else rand(ri + ((BigInt(numShares).pow(n - 1).toInt - 1) * (shareIds.head % 2)) - 1)

            RegEnable(p ^ r, valid)
          }
        pp.suggestName(s"pp_${shareIds.mkString("_")}")
        markDontTouch(pp)
      }(_ ^ _)
    }

    assert(ppMap.size == numShares)

    VecInit((0 until numShares).map(ppMap))
  }
}

class DOM(n: Int, t: Int, width: Int) extends Module {
  require(t >= 1, "masking order must be at least 1")
  require(n > 1, "number of inputs must be at least 2")
  require(width > 0, "width must be at least 1")

  val numShares = t + 1
  val randBits = DOM.numRandBits(n, t)

  println(s"numInputs: $n, order: $t, width: $width, numShares: $numShares, randBits: ${randBits * width}")

  val io = IO(new Bundle {
    val in = Input(Vec(n, Vec(numShares, UInt(width.W))))
    val r = Input(Valid(Vec(randBits, UInt(width.W)))) // TODO
    val out = Output(Valid(Vec(numShares, UInt(width.W))))
  })

  io.out.bits :#= DOM.and(io.in, io.r.bits, io.r.valid)
  io.out.valid := RegNext(io.r.valid, 0.B)

  override def desiredName: String = s"DOM_n${n}_t${t}_w${width}"

  layer.block(layers.Verification) {
    // import chisel3.ltl._
    // import chisel3.ltl.Sequence._
    // AssertProperty(
    //   io.r.valid ### (io.out.reduce(_ ^ _) === RegNext(io.in).map(_.reduce(_ ^ _)).reduce(_ & _)) // `io.in` delayed?
    // )
    when(io.out.valid) {
      assert(io.out.bits.reduce(_ ^ _) === RegNext(io.in).map(_.reduce(_ ^ _)).reduce(_ & _))
    }
  }

}
