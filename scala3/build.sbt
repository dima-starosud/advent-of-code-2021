val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2021",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    // day 15
    libraryDependencies += "org.jgrapht" % "jgrapht-core" % "1.5.1",
    // day 16
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
  )
