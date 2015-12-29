name := "TaskList"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.spire-math" %% "cats" % "0.3.0"
libraryDependencies += "org.specs2" %%  "specs2-core" % "3.6.5" % "test"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)