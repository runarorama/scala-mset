inThisBuild(
  List(
    organization := "com.runar",
    scalaVersion := "2.12.4",
    scalafixConfig := Some(file("project/scalafix.conf"))
  )
)

name := "MSet"
libraryDependencies += "org.typelevel" %% "spire" % "0.15.0"
scalapropsSettings
scalapropsVersion := "0.5.4"
