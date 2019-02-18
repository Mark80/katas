name := "katas"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.4" % Test,
  "com.typesafe.akka" %% "akka-remote" % "2.5.17"
)
libraryDependencies += "org.mockito" % "mockito-core" % "2.10.0" % Test


fork in run := true
