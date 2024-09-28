// package adders

// import chisel3._

// class CarryLookAhead[T <: Data](blockSize: Int) extends Adder[T] {

//   def generate(g: Seq[T], p: Seq[T]): T = {
//     // p and g should be of equal size
//     assert(g.length == p.length)
//     g.indices.map(i => p.drop(i + 1).fold(g(i))(and)).reduce(xor(_, _))
//   }

//   def blockGen0(blockSize: Int, p: Seq[T], g: Seq[T], c: T): (T, T, Seq[T]) = {

//     val p_ = p.reduce(and(_, _))
//     val g_ = generate(g, p)
//     val cg = c +: g
//     val pp = zero +: p // dummy first element
//     val c_ = c +: (1 until blockSize).map(i => generate(cg.slice(0, i + 1), pp.slice(0, i + 1)))
//     (p_, g_, c_)
//   }

//   def blockGenerator(p: Seq[T], g: Seq[T], cin: T): (T, T, Seq[T]) = { // returns p, g, c[]
//     assert(
//       g.length == p.length && g.nonEmpty,
//       s"g.length:${g.length} ==? p.length:${p.length} &&? g.nonEmpty:${g.nonEmpty}"
//     )

//     if (g.length == 1) {
//       return (p.head, g.head, Seq(cin))
//     }


// val bs = if (g.length <= blockSize) g.length else blockSize

  

//     def chunk(i: Int, s: Seq[Bool]) = {
//       val chunkSize = g.length / blockSize
//       s.slice(i * chunkSize, (i + 1) * chunkSize).padTo(chunkSize, 0.B)
//     }

//     var exportedCarries = Seq.empty[Bool]

//     for (i <- 0 until blockSize) {

//       val (p_, g_, c_) = blockGen0(bs, chunk(i, p), chunk(i, g), cin)
      
//       val (pN, gN, blockExportedCarries) =
//         blockGenerator(chunk(i, p), chunk(i, g), carryLookaheadAdderBlock.io.out.c(i))
//       carryLookaheadAdderBlock.io.in.p(i) := pN
//       carryLookaheadAdderBlock.io.in.g(i) := gN

//       exportedCarries = exportedCarries ++ blockExportedCarries
//     }

//     (carryLookaheadAdderBlock.io.out.p, carryLookaheadAdderBlock.io.out.g, VecInit(exportedCarries))
//   }

//   override def add(x: Seq[T], y: T, cin: Bool): T = {
//     val w = Math.min(x.getWidth, y.getWidth)

//     assert(x.getWidth == y.getWidth) // TODO

//     val p = Wire(Vec(w, Bool()))
//     val g = Wire(Vec(w, Bool()))
//     val s = Wire(Vec(w + 1, Bool()))

//     val (pOut, gOut, carries) = blockGenerator(p, g, cin)

//     for (i <- 0 until w) {
//       p(i) := x(i) ^ y(i) // or: transfer
//       g(i) := x(i) & y(i)
//       s(i) := p(i) ^ carries(i)
//       printf(p"x($i)=${x(i)} y($i)=${y(i)} p($i)=${p(i)}\n")
//     }

//     s(w) := gOut | (carries.last & pOut)

//     s.asUInt.asTypeOf(x.pad(math.max(x.getWidth, y.getWidth) + 1))
//   }
// }
