name := """MIT_6_006-introduction-to-algorithm"""

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest"  % "2.2.1"  % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
  "commons-io"     % "commons-io"  % "2.4"
)


fork in run := true