package masking
package conversion
package b2a

import chisel3._

import chest.masking._
import chisel3.util.ShiftRegister
import chest.markDontTouch
import chisel3.util.RegEnable
import chisel3.util.Valid

class B2A extends Module {}

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

  def reg(x: UInt): UInt = markDontTouch(RegNext(markDontTouch(x)))

  def reg(x: UInt, en: Bool): UInt = markDontTouch(RegEnable(markDontTouch(x), en))

  def refresh3(x: BooleanShared, rand: Vec[UInt], en: Bool): Seq[UInt] = {
    require(x.numShares == 2, "Only supports 1st order")

    require(rand.length == 2)

    Seq(x.shares(0) ^ rand(0), x.shares(1) ^ rand(0) ^ rand(1), rand(1))
      .map(reg(_, en))

  }

  def refresh(x: BooleanShared, rand: Vec[UInt], en: Bool): Seq[UInt] = {
    require(rand.length == x.numShares)

    (x.shares.zip(rand).map { case (sh, r) => sh ^ r } :+ rand.reduce(_ ^ _)).map(reg(_, en))

  }

  def apply(x: BooleanShared, rand: Valid[Vec[UInt]]): Seq[UInt] = {
    val a = refresh3(x, rand.bits, rand.valid)
    val b = Seq(phi(a(0), a(2)) ^ a(1), phi(a(0), a(1)), a(1) ^ a(2)).map(reg)
    Seq(b(0) ^ b(1), b(2))
  }

  /** Coron's B2A conversion algorithm
    *
    * delay: 4
    *
    * area: O(n)
    *
    * @param x
    * @param rand
    * @return
    */
  def coron(x: BooleanShared, rand: UInt, pipelined: Boolean = true): Vec[UInt] = {
    require(x.numShares == 2, "CoronB2A only supports 1st order masking")

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
