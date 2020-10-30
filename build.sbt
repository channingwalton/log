name := "log"

version := "0.1"

scalaVersion := "2.13.3"

idePackagePrefix := Some("io.channing")

libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
