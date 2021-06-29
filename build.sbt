import sbt.Keys.libraryDependencies

name := "katas"

version := "0.1"

scalaVersion := "2.12.10"

scalacOptions += "-Ypartial-unification"
scalacOptions += "-language:higherKinds"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value


val http4sVersion = "0.20.1"
val circeVersion = "0.11.1"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

val akkaVersion = "2.5.21"
val akkaHttpVersion = "10.1.7"
val alpakkaVersion = "1.0.5"

libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-sql" % "2.4.4",
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
  "org.mockito" % "mockito-core" % "2.10.0" % Test,
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
  "com.typesafe.akka" %% "akka-stream-kafka" % alpakkaVersion,
  "io.projectreactor" % "reactor-core" % "3.4.3"
)

fork in run := true
