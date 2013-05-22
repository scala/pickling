import sbt._
import Keys._
import scala.util.Properties

object BuildSettings {
  val buildVersion = "1.0.0"
  val buildScalaVersion = "2.10.0" // NOTE: will be 2.10.2
  // path to a build of https://github.com/scalamacros/kepler/tree/topic/pickling
  val picklingParadise = Properties.envOrElse("PICKLING_PARADISE", "/Users/xeno_by/Projects/Kepler_pickling/build/pack")

  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "ch.epfl",
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    scalaHome := Some(file(picklingParadise)),
    unmanagedBase := file(picklingParadise + "/lib"),
    scalacOptions ++= Seq()
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("core"),
    settings = buildSettings
  ) aggregate(core, runtime)

  lazy val core: Project = Project(
    "core",
    file("core"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
    )
  )

  lazy val runtime: Project = Project(
    "runtime",
    file("runtime"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _)
    )
  ) dependsOn(core)
}
