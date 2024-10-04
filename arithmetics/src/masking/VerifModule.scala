package masking

import chisel3._
import chisel3.util._
import chisel3.util.experimental.InlineInstance
import chisel3.util.experimental.FlattenInstance

class VerifModule(numCycles: Int) extends Module with FlattenInstance with InlineInstance {

  val valid = IO(Output(Bool()))

  val inner = Module(new VerifBBox(numCycles)).io
  inner.clock := clock
  inner.reset := reset
  valid := inner.valid

  class VerifBBox(numCycles: Int) extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle {
      val reset = Input(Reset())
      val clock = Input(Clock())
      val valid = Output(Bool())
    })
    // chisel3.Intrinsic("circt_init", VERIF__counter.cloneType, Seq(VERIF__counter))

    override def desiredName: String = simpleClassName(this.getClass()) + numCycles

    setInline(
      desiredName + ".sv",
      s"""
         |module ${desiredName} #(
         |  parameter FORMAL_START_CYCLE = $numCycles
         |) (
         |  input  clock,
         |  input  reset,
         |  output logic valid
         |);
         |
         |`ifndef SYNTHESIS
         |`ifdef FORMAL
         |  reg [$$clog2(FORMAL_START_CYCLE + 1)-1:0] VERIF__counter = 0;
         |  always @(posedge clock) begin
         |    if (!reset && !valid) begin
         |       VERIF__counter <= VERIF__counter + 1;
         |    end
         |  end
         |  assign valid = VERIF__counter == FORMAL_START_CYCLE;
         |`endif
         |`endif
         |
         |endmodule
     """.stripMargin
    )

  }
}