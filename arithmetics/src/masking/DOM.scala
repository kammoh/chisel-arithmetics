package masking

import chisel3._
import chisel3.util._
import chisel3.experimental.skipPrefix

import chest.markDontTouch
import chest.crossProduct
import chest.masking.SharedBool
import chisel3.reflect.DataMirror
import chisel3.experimental.SourceInfo
import chest.masking.Shared

trait Gadget {

  // if not pipelined, the inputs need to remain stable for the maximum delay
  def pipelined: Boolean = true

  /** force balanced delay from all inputs to the output by adding registers
    *
    * @return
    */
  def balanced: Boolean = false

  def andMaxDelay: Int
  def andMinDelay: Int

  def reg[T <: Data](t: T, en: Bool = 1.B)(implicit sourceInfo: SourceInfo): T = markDontTouch(
    RegEnable(markDontTouch(WireDefault(t)), en)
  )
  def reg[T <: Data](t: Option[T], en: Bool)(implicit sourceInfo: SourceInfo): Option[T] = t.map(reg(_, en))
  def reg[T <: Data](t: Option[T])(implicit sourceInfo: SourceInfo): Option[T] = t.map(reg(_))

  def reg[T <: Data](input: T, en: Bool, clear: Bool)(implicit sourceInfo: SourceInfo): T = if (clear.isLit) {
    assert(clear.litValue == 0, "constant clear must be 0")
    reg(input, en)
  } else
    markDontTouch(RegEnable(markDontTouch(Mux(clear, 0.U.asTypeOf(input), input)), en | clear))

  def optReg[T <: Data](u: T, valid: Bool)(implicit sourceInfo: SourceInfo): T =
    if (pipelined) RegEnable(u, valid) else u
  def optReg[T <: Data](u: T)(implicit sourceInfo: SourceInfo): T = if (pipelined) RegEnable(u, 1.B) else u

  def andRandBits(t: Int): Int

  def and(
    a: SharedBool,
    b: SharedBool,
    rand: Seq[Bool],
    randValid: Bool,
    clear: Bool = 0.B,
  )(implicit sourceInfo: SourceInfo): SharedBool

  def and(
    a: Shared,
    b: Shared,
    rand: Seq[Bool]
  )(implicit sourceInfo: SourceInfo): Shared = and(a, b, rand, 1.B)

  def and(
    a: Shared,
    b: Shared,
    rand: Seq[Bool],
    randValid: Bool,
  )(implicit sourceInfo: SourceInfo): Shared = {
    val randPerBit = andRandBits(a.numShares - 1)
    SharedBool.concat(a.asBools.zip(b.asBools).zipWithIndex.map { case ((ai, bi), i) =>
      and(ai, bi, rand.slice(randPerBit * i, randPerBit * (i + 1)), randValid)
    })
  }

  def toffoli(
    a: SharedBool,
    b: SharedBool,
    c: SharedBool,
    rand: Seq[Bool],
    randValid: Bool,
    enable: Option[Bool] = None
  )(implicit sourceInfo: SourceInfo): SharedBool

  def majorityRandBits(t: Int): Int

  def majority(
    a: SharedBool,
    b: SharedBool,
    c: SharedBool,
    rand: Seq[Bool],
    randValid: Bool
  )(implicit sourceInfo: SourceInfo): SharedBool

}

object Gadget {
  def apply(name: String, pipelined: Boolean = true): Gadget = name.toUpperCase match {
    case "DOM" => DOM(pipelined = pipelined)
    case "HPC2" => HPC2(pipelined = pipelined, balanced = false)
    case _ => throw new IllegalArgumentException(s"Unknown gadget: $name")
  }

  /** upper triangular to linear index
    *
    * @param i
    *   row
    * @param j
    *   column
    * @return
    *   flattened index
    */
  def upTriToLin(numShares: Int, i: Int, j: Int): Int = {
    require(0 <= i && i < numShares, s"i must be non-negative and less than $numShares but was $i")
    require(0 <= j && j < numShares, s"j must be non-negative and less than $numShares but was $j")
    require(j != i, s"i and j must be different but got i=$i, j=$j")
    if (j > i) {
      numShares * i - i * (i + 1) / 2 + (j - i - 1)
    } else upTriToLin(numShares, j, i)
  }
}

case class DOM(override val pipelined: Boolean = true) extends Gadget {

  override val balanced: Boolean = true

  def andRandBits(numInputs: Int, t: Int): Int = BigInt(t + 1).pow(numInputs - 1).toInt * t / 2 // FIXME!!!
  def andRandBits(t: Int): Int = (t + 1) * t / 2

  def andMaxDelay: Int = 1
  def andMinDelay: Int = 1

  def majorityRandBits(t: Int): Int = andRandBits(t) + t

  def majority(
    a: SharedBool,
    b: SharedBool,
    c: SharedBool,
    rand: Seq[Bool],
    randValid: Bool
  )(implicit sourceInfo: SourceInfo): SharedBool = {
    val numShares = a.numShares
    require(b.numShares == numShares)
    require(c.numShares == numShares)

    val i0 = reg((a ^ b).refreshed(rand.slice(0, numShares - 1)))

    toffoli(
      (b ^ c),
      i0,
      (b),
      rand.slice(numShares - 1, numShares - 1 + andRandBits(numShares - 1)),
      randValid,
    )
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
        f"${a_name}_AND_${b_name}_"
      else ""

    SharedBool.from((0 until numShares).map { i =>
      (0 until numShares).map { j =>
        if (j == i)
          optReg(a.getShare(i) & b.getShare(i)).suggestName(prefix + s"dom${i}")
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
    enable: Option[Bool] = None
  )(implicit sourceInfo: SourceInfo): SharedBool = {
    val numShares = a.numShares
    require(numShares == b.numShares)

    val requiredRandBits = numShares * (numShares - 1) / 2
    require(rand.length == requiredRandBits, s"rand.length=${rand.length} requiredRandBits=${requiredRandBits}")

    val en = randValid

    def reg[T <: Data](t: T)(implicit sourceInfo: SourceInfo): T = markDontTouch(
      RegEnable(markDontTouch(WireDefault(t)), en)
    )

    def optReg[T <: Data](input: T, en: Bool = en)(implicit sourceInfo: SourceInfo): T =
      if (pipelined || balanced) RegEnable(input, en) else input

    def r(i: Int, j: Int): Bool = rand(Gadget.upTriToLin(numShares, i, j))

    val a_name = DataMirror.queryNameGuess(a)
    val b_name = DataMirror.queryNameGuess(b)
    val c_name = DataMirror.queryNameGuess(c)

    val prefix =
      if (a_name.nonEmpty && a_name != "?" && b_name.nonEmpty && b_name != "?")
        f"${a_name}_AND_${b_name}_XOR_${c_name}_"
      else ""

    SharedBool.from((0 until numShares).map { i =>
      (0 until numShares).map { j =>
        if (j == i)
          optReg(c.getShare(i) ^ (a.getShare(i) & b.getShare(i))).suggestName(prefix + s"dom${i}")
        else
          reg(r(i, j) ^ (a.getShare(i) & b.getShare(j))).suggestName(prefix + s"dom${i}x${j}")
      }.reduce(_ ^ _)
    })
  }

/// FIXME not verified!
  def and(in: Seq[Vec[UInt]], rand: Seq[UInt], valid: Bool): Vec[UInt] = {
    val n = in.length
    val numShares = in.head.length
    require(in.forall(_.length == numShares), "all inputs must have the same number of shares")

    def optReg[T <: Data](input: T): T = super.optReg(input, valid)

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

class DOMModule1(t: Int, width: Int, n: Int = 2) extends Module {

  require(t >= 1, "masking order must be at least 1")
  require(n > 1, "number of inputs must be at least 2")
  require(width > 0, "width must be at least 1")

  def this(t: Int) = this(t, 1, 2)

  val numShares = t + 1

  val g = DOM()
  val randBits = g.andRandBits(n, t)

  println(s"numInputs: $n, order: $t, width: $width, numShares: $numShares, randBits: ${randBits * width}")

  val io = IO(new Bundle {
    val in = Input(Vec(n, Vec(numShares, UInt(width.W))))
    val r = Input(Valid(Vec(randBits, UInt(width.W)))) // TODO
    val out = Output(Valid(Vec(numShares, UInt(width.W))))
  })

  io.out.bits :#= g.and(io.in, io.r.bits, io.r.valid)
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

class DomModule(order: Int) extends Module {
  require(order >= 1, "masking order must be at least 1")

  override def desiredName: String = simpleClassName(this.getClass()) + s"_d${order}"

  val numShares = order + 1

  val w = 1

  def gen = Shared(numShares, w.W)

  val g = DOM(pipelined = false)

  val randPerBit = g.andRandBits(order)

  println(s"numShares: $numShares, randPerBit: $randPerBit")

  val io = IO(new Bundle {
    // val in = Input(Vec(numInputs, gen))
    val a = Input(gen)
    val b = Input(gen)
    // val rand = Flipped(Valid(Vec(HPC2.requiredRandBits(numShares, numInputs), Bool())))
    val rand = Input(UInt((randPerBit * gen.elWidth).W))
    val out = Output(gen)
  })

  io.out :#= g.and(io.a, io.b, io.rand.asBools, randValid = 1.B)

}
