// package adders

// import chisel3._
// import chisel3.util._

// class AdderInput(val w: Int, withCarry: Boolean = false) extends Bundle {

//   val x = UInt(w.W)
//   val y = UInt(w.W)
//   val cin = Option.when(withCarry)(Bool())
// }

// class AdderIO(val w: Int, withCarry: Boolean = false) extends Bundle {
//   val in = Flipped(Decoupled(new AdderInput(w, withCarry)))
//   val out = Decoupled(UInt((w + 1).W))
// }

// class Adder(width: Int, withCarry: Boolean = false) extends Module {
//   def this(width: Int) = this(width, false)

//   val io = IO(
//     new AdderIO(width, withCarry)
//   )

//   // an implicit AdderType needs to be in score
//   implicit val adderType: CarryLookAhead[UInt] = new CarryLookAhead[UInt](4)
//   //  implicit val adderType = new BrentKung[UInt]
//   //  implicit val adderType = new KoggeStone[UInt]

//   // either use as explicit (when need the carry-in):
//   io.out :#= io.in.map(in => adderType.add(in.x, in.y, in.cin.getOrElse(0.B)))
//   // or as implicit (with carry-in tied to 0)
//   //      io.out.bits := io.in.bits.x ++& io.in.bits.y ++& io.in.bits.cin

//   override def desiredName: String = simpleClassName(adderType.getClass) + "Adder" + width
// }
