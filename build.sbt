ThisBuild / scalaVersion := "3.8.4"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "club.libridge"
ThisBuild / organizationName := "libridge"

lazy val root = (project in file("."))
  .settings(
    name := "scalabridge",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.20" % "test",
    libraryDependencies += "org.scalatestplus" %% "junit-5-14" % "3.2.20.0" % "test",
    Test / logBuffered := false,
    scalacOptions ++= Seq(
      "-deprecation", // emit warning and location for usages of deprecated APIs
      "-encoding",
      "utf-8", // Specify character encoding used by source files.
      "-explain", // explain errors in more detail
      "-explain-cyclic", // explain cyclic errors in more detail
      "-explain-types", // explain type errors in more detail
      "-feature", // emit warning and location for usages of features that should be imported explicitly
      "-unchecked", // enable additional warnings where generated code depends on assumptions
      "-Werror", // elevates warnings to errors
      "-Wconf:any:verbose", // shows warning categories
      "-Wconf:id=E176&msg=org.scalatest.*Assertion:s", // Disable Wnonunit-statement warnings related to ScalaTest Assertion.
      "-Wnonunit-statement",
      "-Wunused:imports",
      "-Wunused:privates",
      "-Wunused:locals",
      "-Wunused:params",
      "-Wvalue-discard"
    )
  )
