package masking
package adder

import chest.masking._

class RCAdder(val width: Int, val order: Int = 1)
    extends BooleanMaskedAdderModule
    with adders.RippleCarry[SharedBool] {}
