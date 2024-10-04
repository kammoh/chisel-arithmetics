package masking
package adder

import chest.masking._

class BKAdder(val width: Int, val order: Int = 1) extends BooleanMaskedAdderModule with adders.BrentKung[SharedBool] {}
