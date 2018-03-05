import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Poc Monoid",
    libraryDependencies += scalaTest,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
    scalacOptions ++= Seq(
      "-deprecation", // Warn when deprecated API are used
      "-feature", // Warn for usages of features that should be importer explicitly
      "-unchecked", // Warn when generated code depends on assumptions
      "-Ywarn-dead-code", // Warn when dead code is identified
      "-Ywarn-numeric-widen", // Warn when numeric are widened
      "-Xlint", // Additional warnings (see scalac -Xlint:help)
      "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receive
      "-language:postfixOps",
      "-language:implicitConversions",
      "-language:reflectiveCalls",
      "-language:existentials",
      "-language:higherKinds",
      "-language:experimental.macros"
    )
  )
