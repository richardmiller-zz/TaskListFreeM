name := "TaskList"

version := "1.0"

scalaVersion := "2.11.7"

val libraryVersion = "1.2.0"

libraryDependencies ++= Seq(
  "org.spire-math" %% "cats" % "0.3.0",
  "org.specs2" %%  "specs2-core" % "3.7" % "test",
  "com.github.julien-truffaut"  %%  "monocle-core"    % libraryVersion,
  "com.github.julien-truffaut"  %%  "monocle-generic" % libraryVersion,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % libraryVersion,
  "com.github.julien-truffaut"  %%  "monocle-state"   % libraryVersion,
  "com.github.julien-truffaut"  %%  "monocle-refined" % libraryVersion,
  "com.github.julien-truffaut"  %%  "monocle-law"     % libraryVersion % "test"
)


resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.sonatypeRepo("snapshots")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")
