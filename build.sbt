name := "TaskList"

version := "1.0"

scalaVersion := "2.11.7"

lazy val monocleVersion = "1.1.1"
lazy val doobieVersion = "0.2.3"
lazy val circeVersion = "0.2.1"
lazy val rhoVersion = "0.7.0"
lazy val http4sVersion = "0.10.1"

libraryDependencies ++= Seq(
  "org.spire-math" %% "cats" % "0.3.0",
  "org.specs2" %%  "specs2-core" % "3.6" % "test",
  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-law"     % monocleVersion % "test",
  "org.tpolecat" %% "doobie-core"               % doobieVersion,
  "org.tpolecat" %% "doobie-contrib-h2"        % doobieVersion,
  "org.tpolecat" %% "doobie-contrib-specs2"     % doobieVersion,
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parse" % circeVersion,
  "org.http4s" %% "rho-swagger" % rhoVersion,
  "org.http4s" %% "rho-hal" % rhoVersion,
  "org.http4s" %% "http4s-dsl"          % http4sVersion,  // to use the core dsl
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "ch.qos.logback" % "logback-classic" % "1.1.2"
)


resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.sonatypeRepo("snapshots")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")
