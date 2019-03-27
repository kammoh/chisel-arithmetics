package adders

import chisel3._
import chisel3.core.dontTouch
import chisel3.experimental.chiselName

@chiselName
class UIntCarryLookaheadAdderType(blockSize: Int) extends AdderType[UInt] {

  @chiselName
  class CarryLookaheadAdderBlock(width: Int) extends Module {
    val io = IO(new Bundle {
      val in = new Bundle {
        val c = Input(Bool())
        val p = Input(Vec(width, Bool()))
        val g = Input(Vec(width, Bool()))
      }
      val out = new Bundle {
        val c = Output(Vec(width, Bool()))
        val p = Output(Bool())
        val g = Output(Bool())
      }
    })

    for (i <- 0 until width) {
      dontTouch(io.out.c(i))
    }

    def generate(g: Seq[Bool], p: Seq[Bool]): Bool = {
      // p and g should be of equal size
      assert(g.length == p.length)
      g.indices.map(i => p.drop(i + 1).fold(g(i))(_ & _)).reduce(_ | _)
    }

    io.out.p := io.in.p.reduce(_ & _)
    io.out.g := generate(io.in.g, io.in.p)

    val cg = Seq(io.in.c) ++ io.in.g
    val pp = Seq(0.B) ++ io.in.p // dummy first element

    io.out.c(0) := io.in.c

    for (i <- 1 until width) {
      io.out.c(i) := generate(cg.slice(0, i + 1), pp.slice(0, i + 1))
    }
  }

  def blockGenerator(p: Seq[Bool], g: Seq[Bool], cin: Bool): (Bool, Bool, Seq[Bool]) = { // returns p, g, c[]
    assert(g.length == p.length && g.nonEmpty)

    println(s"cla: g=$g p=$p")

    if (g.length == 1) {
      return (p.head, g.head, Seq(cin))
    }

    val blk = if (g.length <= blockSize) g.length else blockSize
    val carryLookaheadAdderBlock = Module(new CarryLookaheadAdderBlock(width = blk))


    def chunk(i: Int, s: Seq[Bool]) = {
      val chunkSize = g.length / blockSize
      s.slice(i * chunkSize, (i + 1) * chunkSize)
    }

    var exportedCarries = Seq.empty[Bool]

    carryLookaheadAdderBlock.io.in.c := cin

    for (i <- 0 until blockSize) {
      val (pN, gN, blockExportedCarries) = blockGenerator(chunk(i, p), chunk(i, g), carryLookaheadAdderBlock.io.out.c(i))
      carryLookaheadAdderBlock.io.in.p(i) := pN
      carryLookaheadAdderBlock.io.in.g(i) := gN

      exportedCarries = exportedCarries ++ blockExportedCarries
    }

    (carryLookaheadAdderBlock.io.out.p, carryLookaheadAdderBlock.io.out.g, VecInit(exportedCarries))
  }

  override def add(x: UInt, y: UInt, cin: Bool): UInt = {
    val w = Math.min(x.getWidth, y.getWidth)

    assert(x.getWidth == y.getWidth) // TODO

    val p = Wire(Vec(w, Bool()))
    val g = Wire(Vec(w, Bool()))
    val s = Wire(Vec(w + 1, Bool()))

    val (pOut, gOut, carries) = blockGenerator(p, g, cin)

    for (i <- 0 until w) {
      p(i) := x(i) ^ y(i) // or: transfer
      g(i) := x(i) & y(i)
      s(i) := p(i) ^ carries(i)
      printf(p"x($i)=${x(i)} y($i)=${y(i)} p($i)=${p(i)}\n")
    }

    s(w) := gOut | (carries.last & pOut)

    s.asUInt()
  }
}
