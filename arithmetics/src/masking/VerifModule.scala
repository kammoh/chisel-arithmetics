package masking

import chisel3._
import chisel3.util._
import chisel3.util.experimental.InlineInstance
import chisel3.util.experimental.FlattenInstance

class VerifModule(numCycles: Int) extends Module with FlattenInstance with InlineInstance {

  val valid = IO(Output(Bool()))
  val count = IO(Output(UInt(log2Ceil(numCycles + 1).W)))

  val inner = Module(new VerifBBox(numCycles)).io
  inner.clock := clock
  inner.reset := reset
  valid := inner.valid
  count :#= inner.count

  class VerifBBox(numCycles: Int) extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle {
      val reset = Input(Reset())
      val clock = Input(Clock())
      val valid = Output(Bool())
      val count = Output(UInt(log2Ceil(numCycles + 1).W))
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
         |  output logic valid,
         |  output logic [$$clog2(FORMAL_START_CYCLE + 1)-1:0] count
         |);
         |
         |`ifndef SYNTHESIS
         |`ifdef FORMAL
         |  reg [$$clog2(FORMAL_START_CYCLE + 1)-1:0] VERIF__counter = 0;
         |  initial begin
         |    assume(reset);
         |  end
         |  always @(posedge clock) begin
         |    if (!valid) begin
         |       VERIF__counter <= VERIF__counter + 1;
         |    end
         |    if (VERIF__counter != 0) begin
         |      assume(!reset);
         |    end
         |  end
         |  assign valid = VERIF__counter == FORMAL_START_CYCLE;
         |  assign count = VERIF__counter;
         |`else
         |  assign valid = 1'b0;
         |  assign count = 0;
         |`endif
         |`else
         |  assign valid = 1'b0;
         |  assign count = 0;
         |`endif
         |
         |endmodule
     """.stripMargin
    )

  }
}
