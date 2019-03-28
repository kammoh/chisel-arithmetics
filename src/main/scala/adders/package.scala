import chisel3._

import scala.language.implicitConversions

package object adders {

  abstract class AdderType[T <: Data] {
    def add(x: T, y: T, cin: Bool): T
  }


  case class PimpedOpWithCarry[T <: Data](lhsValue:T, op: (T, T, Bool) => T, rhsValue: T, cinValue:Option[Bool]) extends PimpedOp(lhsValue) {

    def +(cin: Bool): T = {
      println("adder has carry!")
      PimpedOpWithCarry(lhsValue, op, rhsValue, Some(cin)).conv
    }

    def ++&(cin: Bool) :T = this + cin

    override def conv : T = op(lhsValue, rhsValue, cinValue match {case Some(cin) => cin; case _ => 0.B } )
  }

  implicit class PimpedOp[T <: Data](val u: T) {

    def ++&(other: T, cin: Bool = 0.B)(implicit adderType: AdderType[T]): PimpedOpWithCarry[T] = {
      println(s"pimped adder ${adderType.getClass.getCanonicalName}")

      PimpedOpWithCarry(this.conv, adderType.add, other, Some(cin) )
    }

    def conv : T = u

  }

  implicit def conv[T <: Data](t: PimpedOp[T]): T = {
    t.conv
  }
}
