name := "hangman"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "1.0.0",
  "com.github.gvolpe" %% "console4cats" % "0.3",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)