import chisel3._

package object adders {

  abstract class AdderType[T <: Data] {
    def add(x: T, y: T, cin: Bool): T
  }

  implicit class PimpedData[T <: Data](u: T) {
    def ++&(other: T)(implicit adderType: AdderType[T]): T = {
      println(s"pimped adder ${adderType.getClass.getCanonicalName}")
      adderType.add(u, other, 0.B)
    }
  }

}
