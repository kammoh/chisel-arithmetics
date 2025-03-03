package masking
package adder

import chest.masking.SharedBool

class BKAdder(val width: Int, val order: Int, gadget: String)
    extends BooleanMaskedAdderModule
    with adders.BrentKung[SharedBool] {
  override def g: Gadget = Gadget(gadget)
}
