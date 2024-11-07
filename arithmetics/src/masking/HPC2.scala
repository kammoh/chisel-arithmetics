package masking

import chisel3._
import chisel3.util._

import chest._
import chest.masking.SharedBool
import chisel3.experimental.SourceInfo


case class HPC2(override val pipelined: Boolean = false, override val balanced: Boolean = false) extends Gadget {
  def andRandBits(t: Int): Int = (t + 1) * t / 2

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
    val en1 = RegNext(en0)

    def optReg[T <: Data](input: T): T = if (pipelined || balanced) RegEnable(input, en0) else input

    def balanceReg[T <: Data](input: T, en: Bool = en0): T = if (balanced) RegEnable(input, en) else input

    def r(i: Int, j: Int): Bool = {
      require(0 <= i && i < numShares)
      require(0 <= j && j < numShares)
      require(j != i)
      if (j > i) {
        val k = numShares * i - i * (i + 1) / 2 + (j - i - 1)
        rand(k)
      } else r(j, i)
    }

    SharedBool.from(Seq.tabulate(numShares) { i =>
      val a_i = balanceReg(a.getShare(i))
      val b_i = optReg(b.getShare(i)) // <-- probably essential for security TODO verify
      // markDontTouch(
      (
        // same-domain terms
        (0 until numShares).map { j =>
          (if (i == j) a_i & b_i else !a_i & reg(r(i, j), en0))
        }.reduce(_ ^ _) +:
          // cross-domain terms
          (0 until numShares).collect {
            case j if j != i =>
              a_i & reg(b.getShare(j) ^ r(i, j), en0)
          }
      ).map(reg(_, en1)).reduce(_ ^ _)
      // )
    })
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
    enable: Option[Bool] = None): SharedBool = {
    val numShares = a.numShares
    require(numShares == b.numShares)

    val requiredRandBits = numShares * (numShares - 1) / 2
    require(rand.length == requiredRandBits, s"rand.length=${rand.length} requiredRandBits=${requiredRandBits}")

    val en0 = enable.map(_ & randValid).getOrElse(randValid)
    val en1 = enable.getOrElse(RegNext(en0))

    // register is required for correctness, based on the pipelining or balancedness
    def optReg[T <: Data](input: T, en: Bool = en0): T = if (pipelined || balanced) RegEnable(input, en) else input

    def balanceReg[T <: Data](input: T, en: Bool = en0): T = if (balanced) RegEnable(input, en) else input

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

    SharedBool.from(Seq.tabulate(numShares) { i =>
      val a_i = if (balanced) optReg(a.getShare(i)) else a.getShare(i)
      val b_i = optReg(b.getShare(i)) // <-- probably essential for security TODO verify
      (
        // same-domain terms
        (balanceReg(c.getShare(i)) +: (0 until numShares).map { j =>
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

  def majority(a: SharedBool, b: SharedBool, c: SharedBool, rand: Seq[Bool], randValid: Bool): SharedBool = {
    val numShares = a.numShares
    require(b.numShares == numShares)
    require(c.numShares == numShares)

    toffoli(
      b ^ c, // 1 cycle delay from `c` (used for carry-in)
      a ^ b, // 2 cycle delay
      b,
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


class Hpc2Module extends Module {
  val order: Int = 1
  val numInputs: Int = 2
  require(order >= 1, "masking order must be at least 1")
  require(numInputs > 1, "number of inputs must be at least 2")
  // require(width > 0, "width must be at least 1")

  override def desiredName: String = simpleClassName(this.getClass()) + s"_order${order}_${numInputs}ins"

  val numShares = order + 1

  def gen = SharedBool(numShares)

  val g = HPC2()

  val io = FlatIO(new Bundle {
    val in = Input(Vec(numInputs, gen))
    // val rand = Flipped(Valid(Vec(HPC2.requiredRandBits(numShares, numInputs), Bool())))
    val rand = Flipped(Vec(g.requiredRandBits(numShares, numInputs), Bool()))
    val out = Output(gen)
  })

  val balanced = false

  numInputs match {
    case 2 =>
      // io.out :#= HPC2.and2(io.in(0), io.in(1), io.rand.bits, io.rand.valid, balanced = balanced)
      io.out :#= g.and(io.in(0), io.in(1), io.rand, randValid = 1.B)
    // case 3 =>
    //   io.out :#= HPC2.and3(io.in(0), io.in(1), io.in(2), io.rand.bits, io.rand.valid, balanced = balanced)
    case _ =>
      throw new NotImplementedError(s"numInputs=${numInputs}")
  }

  val verifDelay = Module(new VerifModule(2))

  // when(~reset.asBool & verifDelay.valid && ShiftRegister(io.rand.valid, 2)) {
  when(~reset.asBool & verifDelay.valid) {
    // printf(p"verifDelay.valid\n")

    assert(
      io.out.shares.reduce(_ ^ _) === RegNext(
        io.in
          .map(_.shares.reduce(_ ^ _))
          .zipWithIndex
          .map { case (in, i) => if (!balanced && i == 0) in else RegNext(in) }
          .reduce(_ & _)
      )
    )
  }
}

class ToffoliModule extends Module {
  val order: Int = 1
  require(order >= 1, "masking order must be at least 1")

  override def desiredName: String = simpleClassName(this.getClass()) + s"_order${order}"

  val numShares = order + 1

  def gen = SharedBool(numShares)

  val g = HPC2()


  val io = IO(new Bundle {
    val a = Input(gen)
    val b = Input(gen)
    val c = Input(gen)
    val rand = Flipped(Valid(Vec(g.requiredRandBits(numShares), Bool())))
    val out = Output(gen)
  })

  val balanced = false

  io.out :#= g.toffoli(io.a, io.b, io.c, io.rand.bits, io.rand.valid)

  val verifDelay = Module(new VerifModule(2))

  when(~reset.asBool & verifDelay.valid && ShiftRegister(io.rand.valid, 2)) {
    // printf(p"verifDelay.valid\n")

    assert(
      io.out.shares.reduce(_ ^ _) === RegNext((io.a.unshared() & RegNext(io.b.unshared())) ^ io.c.unshared())
    )
  }
}
