ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "sudoku",
    libraryDependencies ++= Seq(
      "com.beachape" %% "enumeratum" % "1.7.0",
      "org.scalatest" %% "scalatest" % "3.2.14" % "test"
    )
  )
