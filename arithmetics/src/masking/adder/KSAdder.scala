package masking
package adder

import chest.masking.SharedBool

class KSAdder(val width: Int, val order: Int, gadget: String)
    extends BooleanMaskedAdderModule
    with adders.KoggeStone[SharedBool] {
  override def g = Gadget(gadget)
}
