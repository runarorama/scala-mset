scalapropsSettings

scalapropsVersion := "0.5.4"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.runar",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "MSet",
    libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"
  )
