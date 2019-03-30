name := "katas"

version := "0.1"

scalaVersion := "2.12.4"

scalacOptions += "-Ypartial-unification"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.4" % Test,
  "com.typesafe.akka" %% "akka-remote" % "2.5.17"
)
libraryDependencies += "org.mockito" % "mockito-core" % "2.10.0" % Test
libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.typelevel" %% "cats-core" % "1.6.0"

)


fork in run := true
