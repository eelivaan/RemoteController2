ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.7"

lazy val root = (project in file("."))
  .settings(
    name := "RemoteController"
  )

libraryDependencies += "com.fazecast" % "jSerialComm" % "2.11.2"
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
