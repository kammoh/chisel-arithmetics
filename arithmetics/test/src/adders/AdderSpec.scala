// package adders

// import chisel3._
// import chisel3.experimental.BundleLiterals._
// import chiseltest._
// import org.scalatest.flatspec.AnyFlatSpec
// // import org.scalacheck.Gen
// // import org.scalatest.prop.TableDrivenPropertyChecks

// class AdderSpec extends AnyFlatSpec with ChiselScalatestTester {
//   behavior of "Adder"

//   it should "compute Add" in {

//     // def add(a: BigInt, b: BigInt): BigInt = {
//     //   a + b
//     // }
//     val rnd = scala.util.Random

// //    val widths = for (n <- Gen.choose(2, 128)) yield n

// //    forAll(widths) { w: Int =>
//     val w = 64
//     val numTests = 1000
//     val withCarry = true
//     test(new Adder(w, withCarry = withCarry)) { c =>
//       // val inputInts =
//       //   for (n <- Gen.chooseNum(0L, Math.pow(2, w).toLong - 1, 1))
//       //     yield BigInt(n)

//       val inputs = Seq.fill(numTests)(
//         if (withCarry)
//           new AdderInput(w, withCarry)
//             .Lit(_.x -> BigInt(w, rnd).U, _.y -> BigInt(w, rnd).U, _.cin.get -> BigInt(1, rnd).U)
//         else
//           new AdderInput(w, withCarry)
//             .Lit(_.x -> BigInt(w, rnd).U, _.y -> BigInt(w, rnd).U)
//       )
//       fork {
//         c.io.in.enqueueSeq(inputs)
//       }.fork {
//         c.io.out.expectDequeueSeq(
//           inputs.map(i => (i.x.litValue + i.y.litValue).U + i.cin.getOrElse(0.U))
//         )
//       }.join()
//     }
// //    }
//   }
// }

// //object TestMain extends App1 {
// //
// //  private val manager = new TesterOptionsManager with HasTreadleSuite {
// //    treadleOptions = treadleOptions.copy(
// //      writeVCD = true,
// //      vcdShowUnderscored = true,
// //    )
// //    commonOptions = commonOptions.copy(
// //      targetDirName = "test_main",
// //      topName = "Adder",
// //    )
// //  }
// //
// //  val adderWidth = 8
// //
// //  val testResult = Driver.execute(() => new Adder(adderWidth), manager) {
// //
// //    c =>
// //      new PeekPokeTester(c) {
// //        val rng = scala.util.Random
// //        for (_ <- 0 to 1000) {
// //          val x = rng.nextInt(Math.pow(2, adderWidth).toInt - 1)
// //          val y = rng.nextInt(Math.pow(2, adderWidth).toInt - 1)
// //          val cin = rng.nextInt(2)
// //          poke(c.io.in.bits.x, x)
// //          poke(c.io.in.bits.y, y)
// //          poke(c.io.in.bits.cin, cin)
// //          step(1)
// //          expect(c.io.out.bits, x + y + cin, s"failed for x=$x y=$y cin=$cin")
// //        }
// //      }
// //  }
// //  assert(testResult)
// //  println("SUCCESS!!")
// //}
// //
// //
// //object TestPrefix extends App {
// //  val a = new UIntBrentKungAdderType
// //
// //  val x = (0 until 8).toList
// //
// //  def op(x: Int, y: Int): Int = {
// //    println(s"op $x , $y -> ${x + y}")
// //    x + y
// //  }
// //
// //  println(a.prefix(x, op))
// //}
// //
// //object GenerateVerilogAndDiagram extends App {
// //  val adderWidth = 8
// //
// //  private val manager = new ExecutionOptionsManager("genVerilog") with HasChiselExecutionOptions with HasFirrtlOptions {
// //    commonOptions = commonOptions.copy(
// //      targetDirName = "generate",
// //      topName = "Adder",
// //    )
// //    firrtlOptions.copy(
// //      customTransforms = firrtlOptions.customTransforms :+ new MacroCompilerTransform
// //    )
// //
// //  }
// //
// //  val firrtl = chisel3.Driver.emit(() => new Adder(adderWidth))
// //  val config = Config(
// //    targetDir = manager.targetDirName,
// //    firrtlSource = firrtl,
// //    useRanking = true
// //  )
// //
// //  chisel3.Driver.execute(manager, () => new Adder(adderWidth))
// //
// //  FirrtlDiagrammer.run(config)
// //
// //}
