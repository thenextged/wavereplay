ThisBuild / scalaVersion        := "2.13.7"
ThisBuild / version             := "1.0.0"
ThisBuild / organization        := "stanford-aha"

lazy val root = (project in file("."))
    .settings(
        name := "vcdreplay",
        libraryDependencies ++= Seq(
            "com.github.scopt" %% "scopt" % "4.0.1"
        )
    )
