val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "timeHybrids",
    version := "1.0.0",

    scalaVersion := scala3Version
  )
