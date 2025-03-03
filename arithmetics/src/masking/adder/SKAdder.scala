package masking
package adder

import chest.masking.SharedBool

class SKAdder(val width: Int, val order: Int, gadget: String)
    extends BooleanMaskedAdderModule
    with adders.SklanskyAdder[SharedBool] {
  override def g = Gadget(gadget)
}

class PipelinedSKAdder(val width: Int, val order: Int, gadget: String)
    extends BooleanMaskedAdderModule
    with adders.PipelinedSklanskyAdder[SharedBool] {
  override def g = Gadget(gadget)
}
