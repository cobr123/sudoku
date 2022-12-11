ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

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

lazy val sudoku_scala = (project in file("./sudoku_scala"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "sudoku_scala",
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % "2.19.0",
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.19.0" % Provided,
      "org.scalatest" %% "scalatest" % "3.2.14" % "test",
    )
  )
