name := """deterministic"""

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4",
  "com.typesafe" % "config" % "1.3.0"
)


fork in run := true