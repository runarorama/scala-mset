inThisBuild(
  List(
    organization := "com.runar",
    scalaVersion := "2.12.7"
  )
)

name := "MSet"
libraryDependencies += "org.typelevel" %% "spire" % "0.16.0"
scalapropsSettings
scalapropsVersion := "0.5.4"
