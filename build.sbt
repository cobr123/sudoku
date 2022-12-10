ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("./sudoku_scala_js"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(sudoku_scala)
  .settings(
    name := "sudoku_scala_js",
    // This is an application with a main method
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.1.0",
    ),
  )

val circeVersion = "0.14.3"

lazy val sudoku_scala = (project in file("./sudoku_scala"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "sudoku_scala",
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % circeVersion,
      "io.circe" %%% "circe-generic" % circeVersion,
      "io.circe" %%% "circe-generic-extras" % circeVersion,
      "io.circe" %%% "circe-parser" % circeVersion,
      "com.beachape" %%% "enumeratum" % "1.7.0",
      "com.beachape" %%% "enumeratum-circe" % "1.7.0",
      "org.scalatest" %% "scalatest" % "3.2.14" % "test",
    )
  )
