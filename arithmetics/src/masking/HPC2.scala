package masking

import chisel3._
import chisel3.util._
import chisel3.experimental.skipPrefix

import chest.markDontTouch

object HPC2 {

  /** @param a
    *   low delay input
    * @param b
    *   high delay input
    * @param rand
    * @param en
    * @param clear
    * @return
    *   output has delay of 1 cycle with respect to input `a` and 2 cycles with respect to `b`
    */
  def unbalancedAnd2(a: Vec[Bool], b: Vec[Bool], rand: Vec[Bool], randValid: Bool, clear: Bool = 0.B): Vec[Bool] = {
    require(a.length == b.length)

    def reg[T <: Data](input: T, en: Bool): T =
      markDontTouch(RegEnable(Mux(clear, 0.U.asTypeOf(input), markDontTouch(input)), en | clear))

    object r {
      private lazy val _r = skipPrefix {
        val randIter = rand.iterator
        Seq.tabulate(a.length)(i =>
          Array.tabulate(a.length)(j => {
            require(j > i)
            randIter.next()
          })
        )
      }
      def apply(i: Int, j: Int): Bool = if (j > i) _r(i)(j) else r(j, i)
    }

    val prevRandValid = RegNext(randValid)

    VecInit(a.indices.map { i =>
      (a.indices.map(j => !a(i) & (if (i == j) RegNext(b(i)) else reg(r(i, j), randValid))).reduce(_ & _)
        +: a.indices.filter(_ != i).map { j =>
          a(i) & reg(b(j) ^ r(i, j), randValid)
        }).map(reg(_, prevRandValid)).reduce(_ ^ _)
    })

  }
}

class HPC2(n: Int, t: Int, width: Int) extends Module {

  require(t >= 1, "masking order must be at least 1")
  require(n > 1, "number of inputs must be at least 2")
  require(width > 0, "width must be at least 1")

}
