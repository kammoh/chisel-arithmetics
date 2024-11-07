package masking

import chisel3._
import chisel3.util._
import chisel3.experimental.skipPrefix

import chest.markDontTouch
import chest.crossProduct
import chest.masking.SharedBool
import chisel3.reflect.DataMirror
import chisel3.experimental.SourceInfo

trait Gadget {

  def reg[T <: Data](t: T, en: Bool = 1.B): T = markDontTouch(RegEnable(markDontTouch(WireDefault(t)), en))
  def reg[T <: Data](t: Option[T], en: Bool): Option[T] = t.map(reg(_, en))
  def reg[T <: Data](t: Option[T]): Option[T] = t.map(reg(_))

  def reg[T <: Data](input: T, en: Bool, clear: Bool): T = if (clear.isLit) {
    assert(clear.litValue == 0, "constant clear must be 0")
    reg(input, en)
  } else
    markDontTouch(RegEnable(markDontTouch(Mux(clear, 0.U.asTypeOf(input), input)), en | clear))

  def andRandBits(t: Int): Int

  def and(
    a: SharedBool,
    b: SharedBool,
    rand: Seq[Bool],
    randValid: Bool,
    clear: Bool = 0.B,
    pipelined: Boolean = true,
    balanced: Boolean = false,
  )(implicit sourceInfo: SourceInfo): SharedBool

  def toffoli(
    a: SharedBool,
    b: SharedBool,
    c: SharedBool,
    rand: Seq[Bool],
    randValid: Bool,
    enable: Option[Bool] = None,
    pipelined: Boolean = true, // if not pipelined, the inputs need to remain stable for 2 cycles
    balanced: Boolean = false): SharedBool

  def majorityRandBits(t: Int): Int

  def majority(a: SharedBool, b: SharedBool, c: SharedBool, rand: Seq[Bool], randValid: Bool, pipelined: Boolean = true)
    : SharedBool

}

object DOM extends Gadget {
  def andRandBits(numInputs: Int, t: Int): Int = BigInt(t + 1).pow(numInputs - 1).toInt * t / 2 // FIXME!!!
  def andRandBits(t: Int): Int = (t + 1) * t / 2

  def majorityRandBits(t: Int): Int = andRandBits(t) + t

  def majority(a: SharedBool, b: SharedBool, c: SharedBool, rand: Seq[Bool], randValid: Bool, pipelined: Boolean = true)
    : SharedBool = {
    val numShares = a.numShares
    require(b.numShares == numShares)
    require(c.numShares == numShares)

    val i0 = reg((a ^ b).refreshed(rand.slice(0, numShares - 1)))

    toffoli(
      b ^ c,
      i0,
      b,
      rand.slice(numShares - 1, numShares - 1 + andRandBits(numShares - 1)),
      randValid,
      pipelined = pipelined
    )

    // and(
    //   i0,
    //   i1,
    //   rand.slice(2 * numShares - 2, 2 * numShares - 2 + andRandBits(numShares - 1)),
    //   randValid,
    //   pipelined = pipelined
    // ) ^ b

    // val en = randValid

    // def reg[T <: Data](t: T): T = super.reg(t, en)

    // def optReg[T <: Data](input: T): T = if (pipelined) RegEnable(input, en) else input

    // val a0 = a.getShare(0)
    // val a1 = a.getShare(1)
    // val b0 = b.getShare(0)
    // val b1 = b.getShare(1)
    // val c0 = c.getShare(0)
    // val c1 = c.getShare(1)

    // val r0 = rand(0)
    // val r1 = rand(1)
    // val r2 = rand(2)

    // val x0 = reg(a0 ^ r0) & optReg(b1 ^ c1)
    // val x1 = reg(b0 ^ r1) & optReg(c1 ^ a1)
    // val x2 = reg(c0 ^ r2) & optReg(b1 ^ a1)

    // SharedBool.from(
    //   Seq(
    //     //
    //     optReg(
    //       a0 & b0 ^ a0 & c0 ^ b0 & c0
    //     ), // ^ reg(!b1 & r0) ^ reg(!a1 & r2) ^ reg(!c1 & r1),  ^ !c1 & r0  ^ !a1 & r1  ^ !b1 & r2
    //     optReg(a1 & b1 ^ a1 & c1 ^ b1 & c1) ^ x0 ^ x1 ^ x2
    //   )
    // )
  }

  /// FIXME not verified!
  def and(in: Seq[Vec[UInt]], rand: Seq[UInt], valid: Bool): Vec[UInt] = {
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

  /** @param a
    *   input
    * @param b
    *   input
    * @param rand
    *   fresh random bits
    * @return
    *   masked a & b
    */
  def and(
    a: SharedBool,
    b: SharedBool,
    rand: Seq[Bool],
    randValid: Bool,
    clear: Bool = 0.B,
    pipelined: Boolean = true,
    balanced: Boolean = false,
  )(implicit sourceInfo: SourceInfo): SharedBool = {
    val numShares = a.numShares
    require(b.numShares == numShares)
    require(
      rand.length == andRandBits(numShares - 1),
      s"rand.length=${rand.length} requiredRandBits=${andRandBits(numShares - 1)}"
    )

    val en = randValid

    def reg[T <: Data](t: T): T = super.reg(t, en)

    def optReg[T <: Data](input: T, en: Bool = en): T = if (pipelined) RegEnable(input, en) else input

    def r(i: Int, j: Int): Bool = {
      require(0 <= i && i < numShares)
      require(0 <= j && j < numShares)
      require(j != i)
      if (j > i) {
        val k = numShares * i - i * (i + 1) / 2 + (j - i - 1)
        rand(k)
      } else r(j, i)
    }

    val a_name = DataMirror.queryNameGuess(a)
    val b_name = DataMirror.queryNameGuess(b)

    val prefix =
      if (a_name.nonEmpty && a_name != "?" && b_name.nonEmpty && b_name != "?")
        f"${a_name}_and_${b_name}_"
      else ""

    SharedBool.from((0 until numShares).map { i =>
      (0 until numShares).map { j =>
        if (j == i)
          optReg(a.getShare(i) & b.getShare(i)).suggestName(s"dom${i}")
        else
          reg(r(i, j) ^ (a.getShare(i) & b.getShare(j)))
            .suggestName(
              prefix + s"dom${i}x${j}"
            )
      }.reduce(_ ^ _)
    })
  }

  /** ==Toffoli gate==
    *
    * Computes c ^ (a & b)
    *
    * @param a
    *   AND input.
    * @param b
    *   AND input.
    * @param c
    *   XOR input.
    * @param rand
    * @param en
    * @param clear
    * @return
    *   [c ^ (a & b)]
    *
    * @note
    *   Output delay: 1 cycle
    */
  def toffoli(
    a: SharedBool,
    b: SharedBool,
    c: SharedBool,
    rand: Seq[Bool],
    randValid: Bool,
    enable: Option[Bool] = None,
    pipelined: Boolean = true, // if not pipelined, the inputs need to remain stable for 2 cycles
    balanced: Boolean = false): SharedBool = {
    val numShares = a.numShares
    require(numShares == b.numShares)

    val requiredRandBits = numShares * (numShares - 1) / 2
    require(rand.length == requiredRandBits, s"rand.length=${rand.length} requiredRandBits=${requiredRandBits}")

    val en = randValid

    def reg[T <: Data](t: T): T = markDontTouch(RegEnable(markDontTouch(WireDefault(t)), en))

    def optReg[T <: Data](input: T, en: Bool = en): T = if (pipelined || balanced) RegEnable(input, en) else input

    def r(i: Int, j: Int): Bool = {
      require(0 <= i && i < numShares)
      require(0 <= j && j < numShares)
      require(j != i)
      if (j > i) {
        val k = numShares * i - i * (i + 1) / 2 + (j - i - 1)
        rand(k)
      } else r(j, i)
    }

    SharedBool.from((0 until numShares).map { i =>
      (0 until numShares).map { j =>
        if (j == i)
          optReg(c.getShare(i) ^ (a.getShare(i) & b.getShare(i))).suggestName(s"dom${i}")
        else
          reg(r(i, j) ^ (a.getShare(i) & b.getShare(j))).suggestName(s"dom${i}x${j}")
      }.reduce(_ ^ _)
    })
  }
}

class DOM(n: Int, t: Int, width: Int) extends Module {
  require(t >= 1, "masking order must be at least 1")
  require(n > 1, "number of inputs must be at least 2")
  require(width > 0, "width must be at least 1")

  val numShares = t + 1
  val randBits = DOM.andRandBits(n, t)

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
