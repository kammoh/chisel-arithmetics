package masking
package conversion
package b2a

import chisel3._
import chisel3.layers._
import chisel3.util._

import chest._
import chest.masking._

class B2A(W: Int) extends Module {
  val io = IO(new Bundle {
    val x = Input(BooleanShared(2, UInt(W.W)))
    val rand = Input(UInt(W.W))
    val out = Output(Vec(2, UInt(W.W)))
  })

  io.out :#= B2A.pini(io.x, io.rand)

  layer.block(Verification.Assert) {
    val p = Pipe(!reset.asBool, io.x.shares.reduce(_ ^ _), 1)
    when(p.valid) {
      assert(io.out(0) - io.out(1) === p.bits)
    }
  }
}

object B2A {

  /** $\Phi(a, b) = (a \opus b) + b, a, b \in Z$ ( or $Z_{2^k}$ )
    *
    * is affine over $F_2$
    *
    * $\Phi(a, b) \oplus \Phi(a, 0) = \Phi(a, b) \oplus a$ is linear
    *
    * @param a
    * @param b
    * @return
    */
  def phi(a: UInt, b: UInt): UInt = (a ^ b) + b

  def reg(x: UInt): UInt = markDontTouch(RegEnable(markDontTouch(x), 1.B))

  def pipeReg(x: UInt, delay: Int): UInt = ShiftRegister(x, delay)
  def pipeReg(x: UInt): UInt = RegEnable(x, 1.B)

  def reg(x: UInt, en: Bool): UInt = markDontTouch(RegEnable(markDontTouch(x), en))

  def refresh3(x: BooleanShared, rand: Seq[UInt], en: Bool): Seq[UInt] = {
    require(x.numShares == 2, "Only supports 1st order")

    require(rand.length == 2)

    Seq(x.shares(0) ^ rand(0), x.shares(1) ^ rand(0) ^ rand(1), WireDefault(chiselTypeOf(rand(1)), rand(1)))
      .map(reg(_, en))
  }

  def refresh(x: BooleanShared, rand: Vec[UInt], en: Bool): Seq[UInt] = {
    require(rand.length == x.numShares)

    (x.shares.zip(rand).map { case (sh, r) => sh ^ r } :+ rand.reduce(_ ^ _)).map(reg(_, en))

  }

  def apply(x: BooleanShared, rand: Valid[Vec[UInt]]): Seq[UInt] = {
    val a = refresh3(x, rand.bits, rand.valid)
    println(s"a: ${a.map(_.widthOption)}")
    val b = Seq(phi(a(0), a(2)) ^ a(1), phi(a(0), a(1)), a(1) ^ a(2)).map(reg)
    println(s"b: ${b.map(_.widthOption)}")
    Seq(b(0) ^ b(1), b(2))
  }

  /** 1st order PINI B2A_{2^k} conversion from the paper "Efficient Boolean-to-Arithmetic Mask Conversion" by Shahmirzadi
    * and Hutter, IACR Communications in Cryptography, 2024
    *
    * @param x
    * @param rand
    * @return
    */
  def pini(x: BooleanShared, rand: UInt): Vec[UInt] = {
    require(x.numShares == 2, "This B2A only supports 1st order masking")

    val a1 = pipeReg(x.shares(0))
    val a2 = reg(x.shares(1) + rand)
    val a3 = reg(x.shares(1) ^ rand)

    val z = VecInit(a2(0) +: (1 until x.elWidth).map { i =>
      val ts = (1 until i).map { d =>
        val j = i - d - 1
        a1(j) & a3(j + d, j + 1).andR & ~a3(j)
      }
      a2(i) ^ (a1(i - 1) & ~a3(i - 1)) ^ ts.reduceOption(_ ^ _).getOrElse(0.B)
    }).asUInt

    VecInit(z, reg(x.shares(0) ^ rand))
  }

  def goubin(x: BooleanShared, rand: Seq[UInt]): Seq[UInt] = {
    require(x.numShares == 2, "This B2A only supports 1st order masking")

    // cycle 1
    val a1 = reg(x.shares(0) ^ rand(0))
    val a2 = reg(x.shares(1) ^ rand(0) ^ rand(1))
    val a3 = pipeReg(rand(1))
    val c2 = pipeReg(reg(x.shares(1) ^ rand(0)), 2)
    // cycle 2
    val b1 = reg(phi(a1, a3) ^ a1)
    val b2 = reg(phi(a1, a2))
    // val b3 = reg(a2 ^ a3)
    // cycle 3
    val c1 = reg(b1 ^ b2)
    // val c2 = reg(b3)

    Seq(c1, c2)
  }

  /** Goubin's B2A conversion algorithm
    *
    * delay: 4
    *
    * area: O(n)
    *
    * @param x
    * @param rand
    * @return
    */
  def goubin_bad(x: BooleanShared, rand: UInt, pipelined: Boolean = true): Vec[UInt] = {
    require(x.numShares == 2, "This B2A only supports 1st order masking")

    def optReg(x: UInt, delay: Int = 1): UInt = if (pipelined) ShiftRegister(x, delay) else x

    val x0_1 = optReg(x.shares(0)) //  D0, t: 1

    val t2 = phi(x.shares(0), rand) //   D0
    val t3 = reg(t2 ^ x.shares(0)) //   D0, t: (2, 2) -> 3

    val g1 = reg(x.shares(1) ^ rand) //  D1, t: (0, 0) -> 1
    val a1 = reg(x0_1 ^ g1) //         D0+1, t: (1, 1) -> 2
    val a2 = reg(a1 + optReg(g1)) //   D0+1, t: (2, 2) -> 3
    val a3 = reg(a2 ^ t3) //           D0+1, t: (3, 3) -> 4

    VecInit(a3, optReg(x.shares(1), 4))

  }
}
