name := "arith"

version := "0.1"

scalaVersion := "2.12.8"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"


parallelExecution in Test := false

fork in Test := false

trapExit := false

scalacOptions ++= Seq ("-Xsource:2.11", "-unchecked", "-deprecation", "-feature", "-language:reflectiveCalls")

libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.2-SNAPSHOT"

libraryDependencies += "edu.berkeley.cs" %% "chisel-iotesters" % "1.3-SNAPSHOT"

libraryDependencies += "edu.berkeley.cs" %% "dsptools" % "1.2-SNAPSHOT"

libraryDependencies += "edu.berkeley.cs" %% "firrtl-diagrammer" % "1.1-SNAPSHOT"

libraryDependencies += "edu.berkeley.cs" %% "dsptools" % "latest"

// Local dependencies
libraryDependencies += "edu.berkeley.cs" %% "barstools" % "0.1-SNAPSHOT"


libraryDependencies += "org.scalanlp" %% "breeze" % "0.13+"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "edu.berkeley.cs" %% "chisel-testers2" % "0.1-SNAPSHOT"