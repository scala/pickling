import sbt._
import Keys._
import scala.util.Properties

object BuildSettings {
  val buildVersion = "1.0.0-SNAPSHOT"
  val buildScalaVersion = "2.11.0-SNAPSHOT"
  val buildScalaOrganization = "org.scala-lang.macro-paradise"

  val useLocalBuildOfParadise = false
  // path to a build of https://github.com/scalamacros/kepler/tree/paradise/macros
  val localBuildOfParadise211 = Properties.envOrElse("MACRO_PARADISE211", "/Users/xeno_by/Projects/Paradise/build/pack")

  val buildSettings = Defaults.defaultSettings ++ Seq(
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    scalaOrganization := buildScalaOrganization
  ) ++ (if (useLocalBuildOfParadise) Seq(
    scalaHome := Some(file(localBuildOfParadise211)),
    unmanagedBase := file(localBuildOfParadise211 + "/lib")
  ) else Nil) ++ Seq(
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions ++= Seq("-feature")
  )
}

object MyBuild extends Build {
  import BuildSettings._

  // http://www.scala-sbt.org/release/docs/Extending/Input-Tasks
  def benchTask(benchClass: String) = inputTask((args: TaskKey[Seq[String]]) =>
    (dependencyClasspath in Runtime in benchmark) map { (wrappedProjectCP) => {
      val start = 100000
      val finish = 1000000
      val increment = 100000
      val numRuns = 10

      val projectCP = wrappedProjectCP.map(_.data).mkString(java.io.File.pathSeparatorChar.toString)
      val toolCP = projectCP // TODO: segregate compiler jars from the rest of dependencies
      val libraryCP = projectCP

      for (len <- (start to finish by increment)) {
        import scala.sys.process._
        var shellCommand = Seq(
          "java", "-Dsize=" + len, "-cp", toolCP,
          "scala.tools.nsc.MainGenericRunner", "-cp", libraryCP,
          benchClass, numRuns.toString)
        // println(shellCommand)
        shellCommand.!
      }
    }
  })

  lazy val core: Project = Project(
    "scala-pickling",
    file("core"),
    settings = buildSettings ++ (if (useLocalBuildOfParadise) Nil else Seq(
      libraryDependencies <+= (scalaVersion)(buildScalaOrganization % "scala-reflect" % _),
      libraryDependencies <+= (scalaVersion)(buildScalaOrganization % "scala-compiler" % _)
    )) ++ Seq(
      libraryDependencies += "org.scalatest" % "scalatest_2.11.0-M3" % "1.9.1" % "test",
      libraryDependencies += "org.scalacheck" % "scalacheck_2.11.0-M3" % "1.10.1" % "test",
      conflictWarning in ThisBuild := ConflictWarning.disable,
      parallelExecution in Test := false, // hello, reflection sync!!
      run <<= run in Compile in sandbox, // http://www.scala-sbt.org/release/docs/Detailed-Topics/Tasks
      InputKey[Unit]("travInt") <<= InputKey[Unit]("travInt") in Compile in benchmark
    )
  )

  lazy val sandbox: Project = Project(
    "sandbox",
    file("sandbox"),
    settings = buildSettings ++ Seq(
      sourceDirectory in Compile <<= baseDirectory(root => root),
      sourceDirectory in Test <<= baseDirectory(root => root),
      libraryDependencies += "org.scalatest" % "scalatest_2.11.0-M3" % "1.9.1",
      parallelExecution in Test := false,
      scalacOptions ++= Seq()
      // scalacOptions ++= Seq("-Xprint:typer")
    )
  ) dependsOn(core)

  lazy val benchmark: Project = Project(
    "benchmark",
    file("benchmark"),
    settings = buildSettings ++ Seq(
      sourceDirectory in Compile <<= baseDirectory(root => root),
      sourceDirectory in Test <<= baseDirectory(root => root),
      InputKey[Unit]("travInt") <<= benchTask("TraversableIntBench")
    )
  ) dependsOn(core)
}
