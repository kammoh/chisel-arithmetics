package masking
package adder

import chest.masking._

class KSAdder(val width: Int, val order: Int = 1) extends BooleanMaskedAdderModule with adders.KoggeStone[SharedBool] {

  // val io = IO(new Bundle {
  //   val a = Input(Shared(numShares, width))
  //   val b = Input(Shared(numShares, width))
  //   val sum = Output(Shared(numShares, width + 1))
  // })

  // io.sum :#= add(io.a, io.b)

  // val delay = log2Ceil(width) + 1

  // val verifDelay = Module(new VerifModule(delay))

  // when(~reset.asBool & verifDelay.valid && ShiftRegister(randInValid, delay)) {
  //   // printf(p"verifDelay.valid\n")

  //   assert(io.sum.unshared() === io.a.unshared() + io.b.unshared())
  // }

}
