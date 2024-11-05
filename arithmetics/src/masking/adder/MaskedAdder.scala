package masking
package adder

import chisel3._
import chisel3.util._
import chisel3.experimental.noPrefix

import adders._

import chest.masking._

trait HasRandLedger { self: Module =>

  private val _randsLedger = collection.mutable.ArrayBuffer.empty[Bool]

  val randInValid = Wire(Bool())

  /** request fresh random bits
    *
    * @param w
    *   number of random bits to get
    * @return
    *   random bits as a UInt of width w
    */
  def reqRand(w: Int): UInt = reqRands(w).asUInt

  def reqRands(w: Int): Vec[Bool] = {
    val r = Seq.fill(w)(dontTouch(Wire(Bool())))
    _randsLedger ++= r
    VecInit(r)
  }

  def reqRand(): Bool = reqRand(1).asBool

  def randAlwaysValid: Boolean = true

  atModuleBodyEnd {
    val randBits = _randsLedger.length
    println(s"randBits=${randBits}")

    if (randAlwaysValid)
      noPrefix {
        val rand = IO(Input(UInt(randBits.W))).suggestName("rand")
        _randsLedger.zipWithIndex.foreach { case (r, i) => r :#= rand(i) }
        randInValid := 1.B
      }
    else
      noPrefix {
        val rand = IO(Flipped(Valid(UInt(randBits.W))))
        _randsLedger.zipWithIndex.foreach { case (r, i) => r :#= rand.bits(i) }
        randInValid := rand.valid
      }

  }
}

trait MaskedAdderBase[T] extends Adder[T] {
  def order: Int

  def width: Int

  def numShares: Int

  override def genG(p: Option[T], g: Option[T], c: Option[T]): Option[T] =
    toffoli(p, c, g)
}

trait MaskedAdder extends MaskedAdderBase[SharedBool] with HasRandLedger { self: Module =>

  def order: Int

  def width: Int

  def numShares: Int = order + 1

  def zero = SharedBool.const(0.B, numShares)

  // def add(a: Shared, b: Shared, cin: Option[Shared]): Shared = {
  //   require(a.numShares ==  b.numShares, "Inputs must have same number of shares")
  //   require(a.getWidth == b.getWidth, "Inputs must have the same width")

  //   add(a.asB b, cin)
  // }

  // def grayCellU(p: Shared, g: Shared, c: Shared): Shared = {
  //   VecInit(p.asBools.zip(g.asBools).map { case (pj, gj) => genG(pj, gj, c) }).asUInt
  // }

  val gadget = DOM

  val randBitsPerAnd2 = gadget.andRandBits(order)
  override def xor(a: SharedBool, b: SharedBool): SharedBool = a ^ b

  override def and(a: SharedBool, b: SharedBool): SharedBool = gadget.and(a, b, reqRands(randBitsPerAnd2), randInValid)

  override def toffoli(a: SharedBool, b: SharedBool, c: SharedBool): SharedBool =
    gadget.toffoli(a, b, c, reqRands(randBitsPerAnd2), randInValid)

  override def not(a: SharedBool): SharedBool = ~a

  def add(a: Shared, b: Shared): Shared =
    SharedBool.concat(add(a.asBools, b.asBools, None))

  override def desiredName: String = {
    val clzName = simpleClassName(this.getClass)
    clzName + (if (clzName.toLowerCase.endsWith("adder")) "" else "Adder") + s"_order${order}_w$width"
  }

}
