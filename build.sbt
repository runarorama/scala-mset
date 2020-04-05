inThisBuild(
  List(
    organization := "com.runar",
    scalaVersion := "2.12.11"
  )
)

name := "MSet"
libraryDependencies += "org.typelevel" %% "spire" % "0.16.0"
scalapropsSettings
scalapropsVersion := "0.6.3"
