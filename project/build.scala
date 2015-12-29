import sbt._

object MyBuild extends Build {

  lazy val root = Project("root", file(".")) dependsOn freeToCompose
  lazy val freeToCompose =
    ProjectRef(uri("git://github.com/msiegenthaler/free-to-compose#master"), "core")

}