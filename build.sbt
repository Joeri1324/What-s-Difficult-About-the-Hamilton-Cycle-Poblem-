import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.5",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Main",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "io.spray" %%  "spray-json" % "1.3.5"
  )
