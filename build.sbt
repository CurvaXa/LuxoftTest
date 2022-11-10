ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "LuxoftTest"
  )

val AkkaVersion = "2.7.0"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
  "com.typesafe.akka" %% "akka-stream-testkit" % AkkaVersion % Test,
  "com.lightbend.akka" %% "akka-stream-alpakka-csv" % "5.0.0",
  "com.typesafe.akka" %% "akka-testkit" % AkkaVersion % Test,
  "org.scalatest" %% "scalatest" % "3.2.14" % "test"
)
