package masking
package adder

import chest.masking.SharedBool
import chisel3.util.ShiftRegister
import chisel3.experimental.SourceInfo

class LFAdder(val width: Int, val order: Int, gadget: String)
    extends BooleanMaskedAdderModule
    with adders.LadnerFischer[SharedBool] {
  override def g = Gadget(gadget)


}
