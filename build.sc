import mill._, scalalib._

import $file.chest.common

object ivys {
  val scalaVersion = "2.13.15"

  // run `mill mill.scalalib.Dependency/showUpdates` to check for updates
  val ivyVersions = Map(
    "scala" -> scalaVersion,
    "org.chipsalliance::chisel" -> "7.0.0-M2+138-6d946a64-SNAPSHOT",
    "org.chipsalliance:::chisel-plugin" -> "$chisel",
    "edu.berkeley.cs::chiseltest" -> "7.0.0-M2+42-e6146b3f-SNAPSHOT",
    "org.scalatest::scalatest" -> "3.2.17+",
    "org.scalacheck::scalacheck" -> "1.17.0+",
    "org.scalatestplus::scalacheck-1-17" -> "3.2.17.0",
    "com.outr::scribe" -> "3.15.0",
    "com.lihaoyi::pprint" -> "0.9.0",
    "com.lihaoyi::mainargs" -> "0.7.0",
    "com.lihaoyi::os-lib" -> "0.10.2",
    "org.scala-lang.modules::scala-parallel-collections" -> "1.0.4",
    "org.scala-lang:scala-reflect" -> scalaVersion,
    "org.json4s::json4s-jackson" -> "4.0.7+",
    "org.json4s::json4s-native" -> "4.0.7+",
    "com.chuusai::shapeless" -> "2.3.12",
    "org.rogach::scallop" -> "5.1.0",
    "com.lihaoyi::utest" -> "0.8.2",
    "org.scalanlp::breeze" -> "2.1.0",
    "com.github.jnr:jnr-ffi" -> "2.2.16",
    "org.typelevel::spire" -> "0.18.0",
    "org.nasdanika.core:drawio" -> "2024.9.0",
  )
}

trait HasIvyVersions {
  def ivyVersions: Map[String, String] = ivys.ivyVersions
}

trait MacrosModule extends millbuild.chest.common.CommonMacrosModule with HasIvyVersions {}
trait ChiselModule extends millbuild.chest.common.CommonChiselModule with HasIvyVersions {}

// trait MacrosModule extends CommonScalaModule {
//   override def ivyDeps = super.ivyDeps() ++ Agg(
//     dep("scala-reflect")
//   )

//   override def scalacOptions = super.scalacOptions() ++ Seq(
//     "-language:experimental.macros"
//   )
// }

object arithmetics extends ChiselModule {
  override def ivyDeps = super.ivyDeps() ++ Agg(
    dep("mainargs"),
    dep("scallop"),
    dep("shapeless"),
    dep("spire"),
    dep("drawio"),
  )

  override def moduleDeps = Seq(chest)

  object test extends ScalaTests with TestModule.ScalaTest {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      dep("chiseltest"),
      dep("scalatest"),
      dep("scalacheck"),
    )
  }
}

object chestMacros extends MacrosModule {
  override def millSourcePath = super.millSourcePath / os.up / "chest" / "macros"
  override def ivyDeps = super.ivyDeps() ++ Agg(
  )
}

object chest extends ChiselModule {
  override def millSourcePath = super.millSourcePath / os.up / "chest" / "chest"

  override def ivyDeps = super.ivyDeps() ++ Agg(
    dep("pprint"),
    dep("scallop"),
    dep("mainargs"),
    dep("scribe"),
    dep("json4s-jackson"),
    dep("json4s-native"),
  )
  override def moduleDeps = scala.Seq(chestMacros)
}
