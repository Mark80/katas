name := "katas"

version := "0.1"

scalaVersion := "2.12.6"

scalacOptions += "-Ypartial-unification"
scalacOptions += "-language:higherKinds"

val http4sVersion = "0.20.1"
val circeVersion = "0.11.1"

resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "io.circe" %% "circe-generic" % "0.6.1",
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.scalatest" %% "scalatest" % "3.0.4" % Test,
  "com.typesafe.akka" %% "akka-remote" % "2.5.17",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.typelevel" %% "cats-effect" % "1.2.0",
  "org.typelevel" %% "cats-core" % "1.6.0",
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.sangria-graphql" %% "sangria-relay" % "1.4.2",
  "org.apache.avro" % "avro" % "1.9.1",
  "org.mockito" % "mockito-core" % "2.10.0" % Test)




fork in run := true
