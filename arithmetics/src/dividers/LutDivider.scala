package dividers

import chisel3._

class LutDivider(val width: Int) extends Module {
  val io = IO(new Bundle {
    val dividend = Input(UInt(width.W))
    val divisor = Input(UInt(width.W))
    val quotient = Output(UInt(width.W))
    val remainder = Output(UInt(width.W))
  })

  val lut = Seq.tabulate(width, width)((i, j) =>
    (i, j) match {
      case (0, _) => 0
      case (_, 0) => 0
      case (i, j) => if (i >= j) 1 << (i - j) else 0
    }
  )


}
