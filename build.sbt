inThisBuild(
  List(
    organization := "com.higher-order",
    scalaVersion := "2.12.11"
  )
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

name := "MSet"
version := "0.7.1"
libraryDependencies += "org.typelevel" %% "spire" % "0.17.0-M1"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
scalapropsSettings
scalapropsVersion := "0.6.3"

githubOwner := "runarorama"
githubRepository := "scala-mset"

githubTokenSource := TokenSource.GitConfig("github.token")

