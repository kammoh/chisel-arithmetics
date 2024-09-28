import chisel3._


package object adders {

  trait AdderType[T <: Data with Num[T]] {
  }

  // case class PimpedOpWithCarry[T <: Bits with Num[T]](
  //   lhsValue: T,
  //   op: (T, T, Bool) => T,
  //   rhsValue: T,
  //   cinValue: Option[Bool])
  //     extends PimpedOp(lhsValue) {

  //   def +(cin: Bool): T = {
  //     println("adder has carry!")
  //     PimpedOpWithCarry(lhsValue, op, rhsValue, Some(cin)).conv
  //   }

  //   def ++&(cin: Bool): T = this + cin

  //   override def conv: T = op(lhsValue, rhsValue, cinValue match { case Some(cin) => cin; case _ => 0.B })
  // }

  // implicit class PimpedOp[T <: Bits with Num[T]](val u: T) {

  //   def ++&(other: T, cin: Bool = 0.B)(implicit adderType: AdderType[T]): PimpedOpWithCarry[T] = {
  //     println(s"pimped adder ${adderType.getClass.getCanonicalName}")

  //     PimpedOpWithCarry(this.conv, adderType.add, other, Some(cin))
  //   }

  //   def conv: T = u

  // }

  // implicit def conv[T <: Bits with Num[T]](t: PimpedOp[T]): T = {
  //   t.conv
  // }

  // def toffoli(a: Bool, b: Bool, c: Bool): Bool = (a & b) ^ c

  // def majority(a: Bool, b: Bool, c: Bool): Bool = toffoli(a ^ b, b ^ c, b)
// ((a ^ b) & (b ^ c) ) ^ b  // (a & b) ^ (a & c) ^ (b & c)

}
