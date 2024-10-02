package masking

import chisel3._
import chisel3.util._

import chest._
import chest.masking._
import chisel3.util.experimental.InlineInstance
import chisel3.util.experimental.FlattenInstance

object HPC2 {

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
  def and2(
    a: SharedBool,
    b: SharedBool,
    rand: Vec[Bool],
    randValid: Bool,
    clear: Bool = 0.B,
    pipelined: Boolean = true,
    balanced: Boolean = false): SharedBool = {
    val numShares = a.numShares
    require(numShares == b.numShares)

    val requiredRandBits = numShares * (numShares - 1) / 2
    require(rand.length == requiredRandBits, s"rand.length=${rand.length} requiredRandBits=${requiredRandBits}")

    val en0 = randValid
    val en1 = RegNext(en0)

    def optReg[T <: Data](input: T, en: Bool = en0): T = if (pipelined || balanced) RegEnable(input, en) else input

    def reg[T <: Data](input: T, en: Bool): T =
      markDontTouch(RegEnable(Mux(clear, 0.U.asTypeOf(input), markDontTouch(input)), en | clear))

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
      val a_i = if (balanced) optReg(a.getShare(i)) else a.getShare(i)
      val b_i = reg(b.getShare(i), en0) // <-- probably essential for security TODO verify
      (
        // same-domain terms
        (0 until numShares).map { j =>
          !a_i & (if (i == j) b_i else reg(r(i, j), en0))
        }.reduce(_ & _) +:
          // cross-domain terms
          (0 until numShares).collect {
            case j if j != i =>
              a_i & reg(b.getShare(j) ^ r(i, j), en0)
          }
      ).map(reg(_, en1)).reduce(_ ^ _)
    })
  }

  def requiredRandBits(numShares: Int, numInputs: Int) = {
    numShares * (numShares - 1) / 2 + (numInputs - 2) * numShares * (numShares - 1) // FIXME!!! WRONG!! TEMPORARY!!!
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
    rand: Vec[Bool],
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
    rand: Vec[Bool],
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

class HPC2() extends Module {
  val order: Int = 1
  val numInputs: Int = 3
  require(order >= 1, "masking order must be at least 1")
  require(numInputs > 1, "number of inputs must be at least 2")
  // require(width > 0, "width must be at least 1")

  override def desiredName: String = simpleClassName(this.getClass()) + s"_order${order}_${numInputs}ins"

  val numShares = order + 1

  def gen = SharedBool(numShares)

  val io = IO(new Bundle {
    val in = Input(Vec(numInputs, gen))
    val rand = Flipped(Valid(Vec(HPC2.requiredRandBits(numShares, numInputs), Bool())))
    val out = Output(gen)
  })

  val balanced = false

  numInputs match {
    case 2 =>
      io.out :#= HPC2.and2(io.in(0), io.in(1), io.rand.bits, io.rand.valid, balanced = balanced)
    case 3 =>
      io.out :#= HPC2.and3(io.in(0), io.in(1), io.in(2), io.rand.bits, io.rand.valid, balanced = balanced)
    case _ =>
      throw new NotImplementedError(s"numInputs=${numInputs}")
  }

  val verifDelay = Module(new VerifModule(2))

  when(~reset.asBool & verifDelay.valid && ShiftRegister(io.rand.valid, 2)) {
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

class VerifModule(numCycles: Int) extends Module with FlattenInstance with InlineInstance {

  val valid = IO(Output(Bool()))

  val inner = Module(new VerifBBox(numCycles)).io
  inner.clock := clock
  inner.reset := reset
  valid := inner.valid

  class VerifBBox(numCycles: Int) extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle {
      val reset = Input(Reset())
      val clock = Input(Clock())
      val valid = Output(Bool())
    })
    // chisel3.Intrinsic("circt_init", VERIF__counter.cloneType, Seq(VERIF__counter))

    override def desiredName: String = simpleClassName(this.getClass()) + numCycles

    setInline(
      desiredName + ".sv",
      s"""
         |module ${desiredName} #(
         |  parameter FORMAL_START_CYCLE = $numCycles
         |) (
         |  input  clock,
         |  input  reset,
         |  output logic valid
         |);
         |
         |`ifndef SYNTHESIS
         |`ifdef FORMAL
         |  reg [$$clog2(FORMAL_START_CYCLE + 1)-1:0] VERIF__counter = 0;
         |  always @(posedge clock) begin
         |    if (!reset && !valid) begin
         |       VERIF__counter <= VERIF__counter + 1;
         |    end
         |  end
         |  assign valid = VERIF__counter == FORMAL_START_CYCLE;
         |`endif
         |`endif
         |
         |endmodule
     """.stripMargin
    )

  }
}
