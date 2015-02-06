import sbt._
import Keys._
import java.net.URL

object BuildSettings {
  val buildVersion      = "0.10.0-SNAPSHOT"
  val buildScalaVersion = System.getProperty("scala.version", "2.11.4")
  val javaVersion       = System.getProperty("java.version")
  val buildSettings = Defaults.defaultSettings ++ Seq(
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions ++= Seq("-feature")
  )
}

object MyBuild extends Build {
  import BuildSettings._

  // http://www.scala-sbt.org/release/docs/Extending/Input-Tasks
  def benchTask(benchClass: String, config: Traversable[Int]) = inputTask((args: TaskKey[Seq[String]]) =>
    (dependencyClasspath in Runtime in benchmark) map { (wrappedProjectCP) => {
      val projectCP = wrappedProjectCP.map(_.data).mkString(java.io.File.pathSeparatorChar.toString)
      val toolCP = projectCP // TODO: segregate compiler jars from the rest of dependencies
      val libraryCP = projectCP

      for (len <- config) {
        import scala.sys.process._
        val jdkOptions =
          if (javaVersion.startsWith("1.8")) Seq("-XX:+UseParallelGC") else Seq("-XX:MaxPermSize=512M", "-XX:+UseParallelGC")
        var shellCommand =
          Seq("java", "-Dsize=" + len, "-cp", toolCP, "-Xms1536M", "-Xmx4096M", "-Xss2M") ++ jdkOptions ++
          Seq("scala.tools.nsc.MainGenericRunner", "-cp", libraryCP, benchClass, "10")
        shellCommand.!
      }
    }
  })

  def loadCredentials(): List[Credentials] = {
    val mavenSettingsFile = System.getProperty("maven.settings.file")
    if (mavenSettingsFile != null) {
      println("Loading Sonatype credentials from " + mavenSettingsFile)
      try {
        import scala.xml._
        val settings = XML.loadFile(mavenSettingsFile)
        def readServerConfig(key: String) = (settings \\ "settings" \\ "servers" \\ "server" \\ key).head.text
        List(Credentials(
          "Sonatype Nexus Repository Manager",
          "oss.sonatype.org",
          readServerConfig("username"),
          readServerConfig("password")
        ))
      } catch {
        case ex: Exception =>
          println("Failed to load Maven settings from " + mavenSettingsFile + ": " + ex)
          Nil
      }
    } else {
      // println("Sonatype credentials cannot be loaded: -Dmaven.settings.file is not specified.")
      Nil
    }
  }

  lazy val core: Project = Project(
    "scala-pickling",
    file("core"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _), // for ToolBox
      libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.7" % "test",
      libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.6" % "test",
      libraryDependencies := {
        CrossVersion.partialVersion(scalaVersion.value) match {
          // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
          case Some((2, scalaMajor)) if scalaMajor >= 11 =>
            libraryDependencies.value ++ Seq(
              "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2")
          // in Scala 2.10, quasiquotes are provided by macro-paradise
          case Some((2, 10)) =>
            libraryDependencies.value ++ Seq(
              compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full),
              "org.scalamacros" %% "quasiquotes" % "2.0.1")
        }
      },
      conflictWarning in ThisBuild := ConflictWarning.disable,
      parallelExecution in Test := false, // hello, reflection sync!!
      run <<= run in Compile in sandbox, // http://www.scala-sbt.org/release/docs/Detailed-Topics/Tasks
      // MISC
      InputKey[Unit]("listInt")            <<= InputKey[Unit]("listInt")            in Compile in benchmark,
      InputKey[Unit]("vectorkryo")         <<= InputKey[Unit]("vectorkryo")         in Compile in benchmark,
      InputKey[Unit]("arrayInt")           <<= InputKey[Unit]("arrayInt")           in Compile in benchmark,
      // PAPER
      InputKey[Unit]("travIntPick")        <<= InputKey[Unit]("travIntPick")        in Compile in benchmark,
      InputKey[Unit]("travIntJava")        <<= InputKey[Unit]("travIntJava")        in Compile in benchmark,
      InputKey[Unit]("travIntKryo")        <<= InputKey[Unit]("travIntKryo")        in Compile in benchmark,
      InputKey[Unit]("travIntPickFreeMem") <<= InputKey[Unit]("travIntPickFreeMem") in Compile in benchmark,
      InputKey[Unit]("travIntJavaFreeMem") <<= InputKey[Unit]("travIntJavaFreeMem") in Compile in benchmark,
      InputKey[Unit]("travIntKryoFreeMem") <<= InputKey[Unit]("travIntKryoFreeMem") in Compile in benchmark,
      InputKey[Unit]("travIntPickSize")    <<= InputKey[Unit]("travIntPickSize")    in Compile in benchmark,
      InputKey[Unit]("travIntJavaSize")    <<= InputKey[Unit]("travIntJavaSize")    in Compile in benchmark,
      InputKey[Unit]("travIntKryoSize")    <<= InputKey[Unit]("travIntKryoSize")    in Compile in benchmark,
      InputKey[Unit]("geoTrellisPick")     <<= InputKey[Unit]("geoTrellisPick")     in Compile in benchmark,
      InputKey[Unit]("geoTrellisJava")     <<= InputKey[Unit]("geoTrellisJava")     in Compile in benchmark,
      InputKey[Unit]("geoTrellisKryo")     <<= InputKey[Unit]("geoTrellisKryo")     in Compile in benchmark,
      InputKey[Unit]("evactor1pick")       <<= InputKey[Unit]("evactor1pick")       in Compile in benchmark,
      InputKey[Unit]("evactor1java")       <<= InputKey[Unit]("evactor1java")       in Compile in benchmark,
      InputKey[Unit]("evactor1kryo")       <<= InputKey[Unit]("evactor1kryo")       in Compile in benchmark,
      InputKey[Unit]("evactor2pick")       <<= InputKey[Unit]("evactor2pick")       in Compile in benchmark,
      InputKey[Unit]("evactor2java")       <<= InputKey[Unit]("evactor2java")       in Compile in benchmark,
      InputKey[Unit]("evactor2kryo")       <<= InputKey[Unit]("evactor2kryo")       in Compile in benchmark,
      InputKey[Unit]("sparklrpick")        <<= InputKey[Unit]("sparklrpick")        in Compile in benchmark,
      InputKey[Unit]("sparklrjava")        <<= InputKey[Unit]("sparklrjava")        in Compile in benchmark,
      InputKey[Unit]("sparklrkryo")        <<= InputKey[Unit]("sparklrkryo")        in Compile in benchmark,
      InputKey[Unit]("graphpick")          <<= InputKey[Unit]("graphpick")          in Compile in benchmark,
      InputKey[Unit]("graphjava")          <<= InputKey[Unit]("graphjava")          in Compile in benchmark,
      InputKey[Unit]("graphkryo")          <<= InputKey[Unit]("graphkryo")          in Compile in benchmark,
      organization := "org.scala-lang.modules",
      organizationName := "LAMP/EPFL",
      organizationHomepage := Some(new URL("http://lamp.epfl.ch")),
      publishMavenStyle := true,
      publishArtifact in Test := false,
      publishTo <<= version { v: String =>
        val nexus = "https://oss.sonatype.org/"
        if (v.trim.endsWith("SNAPSHOT"))
          Some("snapshots" at nexus + "content/repositories/snapshots")
        else
          Some("releases" at nexus + "service/local/staging/deploy/maven2")
      },
      pomIncludeRepository := { x => false },
      pomExtra := (
        <url>https://github.com/scala/pickling</url>
        <inceptionYear>2013</inceptionYear>
        <licenses>
          <license>
            <name>BSD-like</name>
            <url>http://www.scala-lang.org/downloads/license.html
            </url>
            <distribution>repo</distribution>
          </license>
        </licenses>
        <scm>
          <url>git://github.com/scala/pickling.git</url>
          <connection>scm:git:git://github.com/scala/pickling.git</connection>
        </scm>
        <developers>
          <developer>
            <id>phaller</id>
            <name>Philipp Haller</name>
            <timezone>+1</timezone>
            <url>http://github.com/phaller</url>
          </developer>
          <developer>
            <id>xeno-by</id>
            <name>Eugene Burmako</name>
            <timezone>+1</timezone>
            <url>http://github.com/xeno-by</url>
          </developer>
          <developer>
            <id>heathermiller</id>
            <name>Heather Miller</name>
            <timezone>+1</timezone>
            <url>http://github.com/heathermiller</url>
          </developer>
        </developers>
      ),
      credentials ++= loadCredentials()
    )
  )

  lazy val sandbox: Project = Project(
    "sandbox",
    file("sandbox"),
    settings = buildSettings ++ Seq(
      sourceDirectory in Compile <<= baseDirectory(root => root),
      sourceDirectory in Test <<= baseDirectory(root => root),
      libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.7",
      parallelExecution in Test := false,
      // scalacOptions ++= Seq()
      scalacOptions ++= Seq("-Xlog-implicits")
      // scalacOptions ++= Seq("-Xprint:typer")
    )
  ) dependsOn(core)

  lazy val benchmark: Project = Project(
    "benchmark",
    file("benchmark"),
    settings = buildSettings ++ Seq(
      sourceDirectory in Compile <<= baseDirectory(root => root),
      sourceDirectory in Test <<= baseDirectory(root => root),
      scalacOptions ++= Seq("-optimise"),
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
      libraryDependencies += "de.javakaffee" % "kryo-serializers" % "0.22",
      libraryDependencies += "com.esotericsoftware.kryo" % "kryo" % "2.20",
      // MISC
      InputKey[Unit]("listInt")            <<= benchTask("ListIntBench", 100000 to 1000000 by 100000),
      InputKey[Unit]("vectorkryo")         <<= benchTask("KryoVectorBench", 100000 to 1000000 by 100000),
      InputKey[Unit]("arrayInt")           <<= benchTask("ArrayIntBench", 500000 to 5000000 by 500000),
      // PAPER
      InputKey[Unit]("travIntPick")        <<= benchTask("TraversableIntBench", 100000 to 1000000 by 100000),
      InputKey[Unit]("travIntJava")        <<= benchTask("TraversableJavaIntBench", 100000 to 1000000 by 100000),
      InputKey[Unit]("travIntKryo")        <<= benchTask("TraversableKryoIntBench", 100000 to 1000000 by 100000),
      InputKey[Unit]("travIntPickFreeMem") <<= benchTask("TraversableIntBenchFreeMem", 100000 to 1000000 by 100000),
      InputKey[Unit]("travIntJavaFreeMem") <<= benchTask("TraversableJavaIntBenchFreeMem", 100000 to 1000000 by 100000),
      InputKey[Unit]("travIntKryoFreeMem") <<= benchTask("TraversableKryoIntBenchFreeMem", 100000 to 1000000 by 100000),
      InputKey[Unit]("travIntPickSize")    <<= benchTask("TraversableIntBenchSize", 100000 to 1000000 by 100000),
      InputKey[Unit]("travIntJavaSize")    <<= benchTask("TraversableJavaIntBenchSize", 100000 to 1000000 by 100000),
      InputKey[Unit]("travIntKryoSize")    <<= benchTask("TraversableKryoIntBenchSize", 100000 to 1000000 by 100000),
      InputKey[Unit]("geoTrellisPick")     <<= benchTask("GeoTrellisPicklingBench", 5000000 to 50000000 by 5000000),
      InputKey[Unit]("geoTrellisJava")     <<= benchTask("GeoTrellisJavaBench", 5000000 to 50000000 by 5000000),
      InputKey[Unit]("geoTrellisKryo")     <<= benchTask("GeoTrellisKryoBench", 5000000 to 50000000 by 5000000),
      InputKey[Unit]("evactor1pick")       <<= benchTask("EvactorPicklingBench", 1000 to 10000 by 1000),
      InputKey[Unit]("evactor1java")       <<= benchTask("EvactorJavaBench", 1000 to 10000 by 1000),
      InputKey[Unit]("evactor1kryo")       <<= benchTask("EvactorKryoBench", 1000 to 10000 by 1000),
      InputKey[Unit]("evactor2pick")       <<= benchTask("EvactorPicklingBench", 20000 to 40000 by 2000),
      InputKey[Unit]("evactor2java")       <<= benchTask("EvactorJavaBench", 20000 to 40000 by 2000),
      InputKey[Unit]("evactor2kryo")       <<= benchTask("EvactorKryoBench", 20000 to 40000 by 2000),
      InputKey[Unit]("sparklrpick")        <<= benchTask("SparkLRPicklingBench", 20000 to 40000 by 2000),
      InputKey[Unit]("sparklrjava")        <<= benchTask("SparkLRJavaBench", 20000 to 40000 by 2000),
      InputKey[Unit]("sparklrkryo")        <<= benchTask("SparkLRKryoBench", 20000 to 40000 by 2000),
      InputKey[Unit]("graphpick")          <<= benchTask("WikiGraphPicklingBench", 5000 to 14000 by 1000),
      InputKey[Unit]("graphjava")          <<= benchTask("WikiGraphJavaBench", 5000 to 14000 by 1000),
      InputKey[Unit]("graphkryo")          <<= benchTask("WikiGraphKryoBench", 5000 to 14000 by 1000)
    )
  ) dependsOn(core)
}
