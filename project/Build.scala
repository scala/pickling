import sbt._
import Keys._
import scala.util.Properties

object BuildSettings {
  val buildVersion = "1.0.0"
  val buildScalaVersion = "2.10.2-SNAPSHOT"
  val buildScalaOrganization = "org.scala-lang.macro-paradise"
  // path to a build of https://github.com/scalamacros/kepler/tree/paradise/macros210
  // val buildScalaVersion = "2.10.0"
  // val buildScalaOrganization = "org.scala-lang"
  // val paradise210 = Properties.envOrElse("MACRO_PARADISE210", "/Users/xeno_by/Projects/Paradise210/build/pack")

  val buildSettings = Defaults.defaultSettings ++ Seq(
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    scalaOrganization := buildScalaOrganization,
    // scalaHome := Some(file(paradise210)),
    // unmanagedBase := file(paradise210 + "/lib"),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
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
      libraryDependencies <+= (scalaVersion)(buildScalaOrganization % "scala-reflect" % _),
      libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"
    )
  )

  lazy val runtime: Project = Project(
    "runtime",
    file("runtime"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)(buildScalaOrganization % "scala-reflect" % _),
      libraryDependencies <+= (scalaVersion)(buildScalaOrganization % "scala-compiler" % _)
    )
  ) dependsOn(core)
}
