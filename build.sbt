inThisBuild(
  List(
    organization := "com.higher-order",
    scalaVersion := "2.12.11"
  )
)

name := "MSet"
version := "0.2.1"
libraryDependencies += "org.typelevel" %% "spire" % "0.16.2"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
scalapropsSettings
scalapropsVersion := "0.6.3"

githubOwner := "runarorama"
githubRepository := "scala-mset"

githubTokenSource := TokenSource.GitConfig("github.token")

