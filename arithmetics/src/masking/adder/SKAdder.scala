package masking
package adder

import chest.masking._

class SKAdder(val width: Int, val order: Int = 1)
    extends BooleanMaskedAdderModule
    with adders.SklanskyAdder[SharedBool]

class PipelinedSKAdder(val width: Int, val order: Int = 1)
    extends BooleanMaskedAdderModule
    with adders.PipelinedSklanskyAdder[SharedBool]
