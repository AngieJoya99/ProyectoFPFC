ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name := "ProyectoFPFC"
  )

  libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

