ThisBuild / scalaVersion        := "2.13.7"
ThisBuild / version             := "1.0.0"
ThisBuild / organization        := "edu.stanford.aha"

lazy val root = (project in file("."))
    .settings(
        name := "wavereplay",
        libraryDependencies ++= Seq(
            "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
            "org.scalatest" %% "scalatest" % "3.2.11"
        )
    )
