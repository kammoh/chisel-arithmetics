package masking

import chisel3._
import chisel3.util._

import chest._
import chest.masking.{SharedBool, Shared}
import chisel3.experimental.SourceInfo
import chisel3.layers.Verification

case class HPC2(override val pipelined: Boolean = false, override val balanced: Boolean = false) extends Gadget {
  def andRandBits(t: Int): Int = (t + 1) * t / 2

  def andMaxDelay: Int = 2
  def andMinDelay: Int = if (balanced) 2 else 1

  def majorityRandBits(t: Int): Int = andRandBits(t)

  /** @param a
    *   low delay input
    * @param b
    *   high delay input
    * @param rand
    * @param en
    * @param clear
    * @return
    *   masked a & b;
    *
    * if balanced == false: output has delay of 1 cycle with respect to input `a` and 2 cycles with respect to `b`
    */
  def and(
    a: SharedBool,
    b: SharedBool,
    rand: Seq[Bool],
    randValid: Bool,
    clear: Bool = 0.B,
  )(implicit sourceInfo: SourceInfo): SharedBool = {
    val numShares = a.numShares
    require(numShares == b.numShares)

    val requiredRandBits = numShares * (numShares - 1) / 2
    require(rand.length == requiredRandBits, s"rand.length=${rand.length} requiredRandBits=${requiredRandBits}")

    val en0 = randValid

    def oReg[T <: Data](input: T, doReg: Boolean = true): T = if (doReg) RegEnable(input, 1.B) else input

    def r(i: Int, j: Int): Bool = {
      require(0 <= i && i < numShares)
      require(0 <= j && j < numShares)
      require(j != i)
      if (j < i) {
        val k = i * (i + 1) / 2 + j - i
        rand(k)
      } else r(j, i)
    }

    SharedBool.from(Seq.tabulate(numShares) { i =>
      val a_i = a.getShare(i)
      val b_i = b.getShare(i)
      (
        // same-domain terms
        (0 until numShares).map { j =>
          if (i == j)
            if (balanced)
              oReg(a_i & b_i)
            else
              (a_i & oReg(b_i))
          else if (balanced)
            oReg(!a_i & r(i, j))
          else
            !a_i & oReg(r(i, j))
        }.reduce(_ ^ _) +:
          // cross-domain terms
          (0 until numShares).collect {
            case j if j != i =>
              val b_j = b.getShare(j)
              oReg(a_i, balanced) & reg(b_j ^ r(i, j), en0)
          }
      ).map(reg(_)).reduce(_ ^ _)
    })

    // SharedBool.from(Seq.tabulate(numShares) { i =>
    //   val a_i = a.getShare(i)
    //   xorReduceSeq(optReg(balanceReg(a_i) & b.getShare(i)) +: (0 until numShares).filter(_ != i).flatMap { j =>
    //     // val u_ij = reg(!a_i & r_ij)
    //     val s = reg(b.shares(j) ^ r(i, j))
    //     val p1 = reg(s & balanceReg(a_i))
    //     val p0 = balanceReg(reg(!a_i & r(i, j)))

    //     Seq(p0, p1)

    //   })
    // })
  }

  /** ==Toffoli gate==
    *
    * Computes c ^ (a & b)
    *
    * @param a
    *   The first irst AND input. Its delay to the output is 1 cycle.
    * @param b
    *   The second AND input. Its delay to the output is 2 cycles.
    * @param c
    *   delay 1 XOR input
    * @param rand
    * @param en
    * @return
    *   [c ^ (a & [b])]
    * @note
    * ==Output delay==
    *   - from `a`, `c`: 1 cycle
    *   - from `b`: 2 cycles
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

    val en0 = enable.map(_ & randValid).getOrElse(randValid)
    val en1 = enable.getOrElse(RegNext(en0))

    // register is required for correctness, based on the pipelining or balancedness
    def optReg[T <: Data](input: T, en: Bool = en0): T = if (pipelined || balanced) RegEnable(input, en) else input

    def balanceReg[T <: Data](input: T, en: Bool = en0): T = if (balanced && pipelined) RegEnable(input, en) else input

    def r(i: Int, j: Int): Bool = {
      if (j < i)
        r(j, i)
      else {
        require(0 <= i && i < numShares && 0 <= j && j < numShares)
        require(j != i)
        val k = numShares * i - i * (i + 1) / 2 + (j - i - 1)
        rand(k)
      }
    }

    println(s"balanced: $balanced, pipelined: $pipelined")

    SharedBool.from(Seq.tabulate(numShares) { i =>
      val a_i = balanceReg(a.getShare(i))
      val b_i = optReg(b.getShare(i))
      val c_i = balanceReg(c.getShare(i))
      (
        // same-domain terms
        (c_i +: (0 until numShares).map { j =>
          (if (i == j) a_i & b_i else !a_i & reg(r(i, j), en0))
        }).reduce(_ ^ _) +:
          // cross-domain terms
          (0 until numShares).collect {
            case j if j != i =>
              a_i & reg(b.getShare(j) ^ r(i, j), en0)
          }
      ).map(reg(_, en1)).reduce(_ ^ _)
    })
  }

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

    val br = optReg(b)

    toffoli(
      br ^ c, // 1 cycle delay from `c` (used for carry-in)
      a ^ b, // 2 cycle delay
      br, // 1 cycle delay, therefore needs optReg
      rand,
      randValid,
    )
  }

  def requiredRandBits(numShares: Int, multDegree: Int = 2) = {
    numShares * (numShares - 1) / 2 + (multDegree - 2) * numShares * (numShares - 1) // FIXME!!! WRONG!! TEMPORARY!!!
  }

  /** @param a
    *   low delay input
    * @param b
    *   high delay input
    * @param rand
    * @param en
    * @param clear
    * @return
    *   if balanced == false: output has delay of 1 cycle with respect to input `a` and 2 cycles with respect to `b`
    */
  def and3(
    a: SharedBool,
    b: SharedBool,
    c: SharedBool,
    rand: Seq[Bool],
    randValid: Bool,
    clear: Bool = 0.B,
    pipelined: Boolean = true,
    balanced: Boolean = false): SharedBool = {

    require(a.hasSameNumShares(b, c))

    val numShares = a.numShares

    val en0 = randValid
    val en1 = RegNext(en0)

    def optReg[T <: Data](input: T, en: Bool = en0): T = if (pipelined || balanced) RegEnable(input, en) else input

    def reg[T <: Data](input: T, en: Bool): T =
      markDontTouch(RegEnable(Mux(clear, 0.U.asTypeOf(input), markDontTouch(input)), en | clear))

    require(numShares == 2, "TODO: implement for numShares > 2")

    val reqRand = requiredRandBits(numShares, 3)
    require(rand.length == reqRand, s"rand.length=${rand.length} requiredRandBits=${reqRand}")

    // def r(i: Int, j: Int): Bool = {
    //   require(0 <= i && i < numShares)
    //   require(0 <= j && j < numShares)
    //   require(j != i)
    //   if (j > i) {
    //     val k = numShares * i - i * (i + 1) / 2 + (j - i - 1)
    //     rand(k)
    //   } else r(j, i)
    // }

    // TODO
    def r(i: Int): Bool = rand(i)

    val rr = reg(rand.xorReduce, en0)

    SharedBool.from(Seq.tabulate(numShares) { i =>
      val a_i = if (balanced) optReg(a.getShare(i)) else a.getShare(i)
      val b_i = b.getShare(i)
      val bb_i = optReg(b_i) // <-- TODO: !!CRITICAL!! Verify! Make sure reg is not essential for security
      val c_i = c.getShare(i)
      val cc_i = optReg(c_i) // <-- TODO: !!CRITICAL!! Verify! Make sure reg is not essential for security

      val tt: Bool = reg(Seq(b_i & c_i, b_i & r(0), c_i & r(1), r(2)).reduce(_ ^ _), en0)
      val t0 = (a_i & tt) ^ rr

      (
        t0 +: (0 until numShares).collect {
          case j if j != i =>
            val b_j = b.getShare(j)
            val c_j = c.getShare(j)
            Seq(
              a_i & reg(b_j ^ r(1), en0) & cc_i,
              a_i & reg(c_j ^ r(0), en0) & bb_i,
              a_i & reg((b_j & c_j) ^ r(2), en0),
            )
        }.flatten
      ).map(reg(_, en1)).reduce(_ ^ _)
    })
  }

  /** @param a
    *   low delay input 1
    * @param b
    *   high delay input 2
    * @param c
    *   high delay input 3
    * @param c
    *   xored with the output
    * @param rand
    * @param en
    * @param clear
    * @return
    *   if balanced == false: output has delay of 1 cycle with respect to input `a` and 2 cycles with respect to `b`
    */
  def and3Xor(
    a: SharedBool,
    b: SharedBool,
    c: SharedBool,
    d: SharedBool,
    rand: Seq[Bool],
    randValid: Bool,
    clear: Bool = 0.B,
    pipelined: Boolean = true,
    balanced: Boolean = false): SharedBool = {

    require(a.hasSameNumShares(b, c))

    val numShares = a.numShares

    val en0 = randValid
    val en1 = RegNext(en0)

    def optReg[T <: Data](input: T, en: Bool = en0): T = if (pipelined || balanced) RegEnable(input, en) else input

    def reg[T <: Data](input: T, en: Bool): T =
      markDontTouch(RegEnable(Mux(clear, 0.U.asTypeOf(input), markDontTouch(input)), en | clear))

    require(numShares == 2, "TODO: implement for numShares > 2")

    val reqRand = requiredRandBits(numShares, 3)
    require(rand.length == reqRand, s"rand.length=${rand.length} requiredRandBits=${reqRand}")

    // def r(i: Int, j: Int): Bool = {
    //   require(0 <= i && i < numShares)
    //   require(0 <= j && j < numShares)
    //   require(j != i)
    //   if (j > i) {
    //     val k = numShares * i - i * (i + 1) / 2 + (j - i - 1)
    //     rand(k)
    //   } else r(j, i)
    // }

    // TODO
    def r(i: Int): Bool = rand(i)

    val rr = reg(rand.xorReduce, en0)

    SharedBool.from(Seq.tabulate(numShares) { i =>
      val a_i = if (balanced) optReg(a.getShare(i)) else a.getShare(i)
      val d_i = if (balanced) optReg(d.getShare(i)) else d.getShare(i)
      val b_i = b.getShare(i)
      val bb_i = optReg(b_i) // <-- TODO: !!CRITICAL!! Verify! Make sure reg is not essential for security
      val c_i = c.getShare(i)
      val cc_i = optReg(c_i) // <-- TODO: !!CRITICAL!! Verify! Make sure reg is not essential for security

      val tt: Bool = reg(Seq(b_i & c_i, b_i & r(0), c_i & r(1), r(2)).reduce(_ ^ _), en0)
      val t0 = (a_i & tt) ^ rr ^ d_i

      (
        t0 +: (0 until numShares).collect {
          case j if j != i =>
            val b_j = b.getShare(j)
            val c_j = c.getShare(j)
            Seq(
              a_i & reg(b_j ^ r(1), en0) & cc_i,
              a_i & reg(c_j ^ r(0), en0) & bb_i,
              a_i & reg((b_j & c_j) ^ r(2), en0),
            )
        }.flatten
      ).map(reg(_, en1)).xorReduce
    })
  }

}

class Hpc2Module(order: Int, w: Int = 1, balanced: Boolean = true) extends Module {
  // val numInputs: Int = 2
  require(order >= 1, "masking order must be at least 1")
  // require(numInputs > 1, "number of inputs must be at least 2")
  // require(width > 0, "width must be at least 1")

  override def desiredName: String = simpleClassName(this.getClass()) + s"_order${order}"

  val numShares = order + 1

  def gen = Shared(numShares, w.W)

  val g = HPC2(balanced = balanced)

  val randPerBit = g.requiredRandBits(numShares)

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

  layer.block(Verification) {
    val verifDelay = Module(new VerifModule(2))

    when(verifDelay.valid) {
      assert(
        io.out.unshared() === ShiftRegister(
          ShiftRegister(io.b.unshared(), if (balanced) 0 else 1) & io.a.unshared(),
          if (balanced) 2 else 1
        )
      )
    }
  }
}

class Hpc2ToffoliModule extends Module {
  val order: Int = 1
  require(order >= 1, "masking order must be at least 1")

  override def desiredName: String = simpleClassName(this.getClass()) + s"_order${order}"

  val numShares = order + 1

  def gen = SharedBool(numShares)

  val balanced = false
  val pipelined = true

  val g = HPC2(pipelined = pipelined, balanced = balanced)

  val io = IO(new Bundle {
    val a = Input(gen)
    val b = Input(gen)
    val c = Input(gen)
    val rand = Input(Vec(g.requiredRandBits(numShares), Bool()))
    val out = Output(gen)
  })

  io.out :#= g.toffoli(io.a, io.b, io.c, io.rand, 1.B)

  layer.block(Verification.Assert) {
    val a_us_delayed = ShiftRegister(io.a.unshared(), if (balanced) 2 else 1)
    val c_us_delayed = ShiftRegister(io.c.unshared(), if (balanced) 2 else 1)
    val bp = Pipe(!reset.asBool, io.b.unshared(), 2)
    when(bp.valid) {
      assert(io.out.unshared() === (bp.bits & a_us_delayed) ^ c_us_delayed)
    }
  }
  if (!pipelined) {
    layer.block(Verification.Assume) {
      when(!reset.asBool) {
        assume(io.a === RegNext(io.a))
        assume(io.b === RegNext(io.b))
        assume(io.c === RegNext(io.c))
      }
    }
  }
}
