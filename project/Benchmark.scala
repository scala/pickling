import sbt._
import Keys._
import complete.DefaultParsers._
import Util._

object Benchmark {
  // http://www.scala-sbt.org/0.13/docs/Input-Tasks.html
  def benchTask(benchClass: String, config: Traversable[Int]) = Def.inputTask {
    val args: Seq[String] = spaceDelimited("<arg>").parsed
    val wrappedProjectCP = (dependencyClasspath in Runtime).value
    val projectCP = wrappedProjectCP.map(_.data).mkString(java.io.File.pathSeparatorChar.toString)
    val toolCP = projectCP // TODO: segregate compiler jars from the rest of dependencies
    val libraryCP = projectCP

    for (len <- config) {
      val jdkOptions =
        if (javaVersion.startsWith("1.8")) Seq("-XX:+UseParallelGC") else Seq("-XX:MaxPermSize=512M", "-XX:+UseParallelGC")
      var shellCommand =
        Seq("java", "-Dsize=" + len, "-cp", toolCP, "-Xms1536M", "-Xmx4096M", "-Xss2M") ++ jdkOptions ++
        Seq("scala.tools.nsc.MainGenericRunner", "-cp", libraryCP, benchClass, "10")
      shellCommand.!
    }
  }

  lazy val listInt            = inputKey[Unit]("")
  lazy val vectorKryo         = inputKey[Unit]("")
  lazy val arrayInt           = inputKey[Unit]("")

  lazy val travIntPick        = inputKey[Unit]("")
  lazy val travIntJava        = inputKey[Unit]("")
  lazy val travIntKryo        = inputKey[Unit]("")
  lazy val travIntPickFreeMem = inputKey[Unit]("")
  lazy val travIntJavaFreeMem = inputKey[Unit]("")
  lazy val travIntKryoFreeMem = inputKey[Unit]("")
  lazy val travIntPickSize    = inputKey[Unit]("")
  lazy val travIntJavaSize    = inputKey[Unit]("")
  lazy val travIntKryoSize    = inputKey[Unit]("")
  lazy val geoTrellisPick     = inputKey[Unit]("")
  lazy val geoTrellisJava     = inputKey[Unit]("")
  lazy val geoTrellisKryo     = inputKey[Unit]("")
  lazy val evactor1Pick       = inputKey[Unit]("")
  lazy val evactor1Java       = inputKey[Unit]("")
  lazy val evactor1Kryo       = inputKey[Unit]("")
  lazy val evactor2Pick       = inputKey[Unit]("")
  lazy val evactor2Java       = inputKey[Unit]("")
  lazy val evactor2Kryo       = inputKey[Unit]("")
  lazy val sparklrPick        = inputKey[Unit]("")
  lazy val sparklrJava        = inputKey[Unit]("")
  lazy val sparklrKryo        = inputKey[Unit]("")
  lazy val graphPick          = inputKey[Unit]("")
  lazy val graphJava          = inputKey[Unit]("")
  lazy val graphKryo          = inputKey[Unit]("")

  def benchmarkSettings = Seq(
    // MISC
    listInt            := benchTask("ListIntBench", 100000 to 1000000 by 100000).evaluated,
    vectorKryo         := benchTask("KryoVectorBench", 100000 to 1000000 by 100000).evaluated,
    arrayInt           := benchTask("ArrayIntBench", 500000 to 5000000 by 500000).evaluated,
    // PAPER
    travIntPick        := benchTask("TraversableIntBench", 100000 to 1000000 by 100000).evaluated,
    travIntJava        := benchTask("TraversableJavaIntBench", 100000 to 1000000 by 100000).evaluated,
    travIntKryo        := benchTask("TraversableKryoIntBench", 100000 to 1000000 by 100000).evaluated,
    travIntPickFreeMem := benchTask("TraversableIntBenchFreeMem", 100000 to 1000000 by 100000).evaluated,
    travIntJavaFreeMem := benchTask("TraversableJavaIntBenchFreeMem", 100000 to 1000000 by 100000).evaluated,
    travIntKryoFreeMem := benchTask("TraversableKryoIntBenchFreeMem", 100000 to 1000000 by 100000).evaluated,
    travIntPickSize    := benchTask("TraversableIntBenchSize", 100000 to 1000000 by 100000).evaluated,
    travIntJavaSize    := benchTask("TraversableJavaIntBenchSize", 100000 to 1000000 by 100000).evaluated,
    travIntKryoSize    := benchTask("TraversableKryoIntBenchSize", 100000 to 1000000 by 100000).evaluated,
    geoTrellisPick     := benchTask("GeoTrellisPicklingBench", 5000000 to 50000000 by 5000000).evaluated,
    geoTrellisJava     := benchTask("GeoTrellisJavaBench", 5000000 to 50000000 by 5000000).evaluated,
    geoTrellisKryo     := benchTask("GeoTrellisKryoBench", 5000000 to 50000000 by 5000000).evaluated,
    evactor1Pick       := benchTask("EvactorPicklingBench", 1000 to 10000 by 1000).evaluated,
    evactor1Java       := benchTask("EvactorJavaBench", 1000 to 10000 by 1000).evaluated,
    evactor1Kryo       := benchTask("EvactorKryoBench", 1000 to 10000 by 1000).evaluated,
    evactor2Pick       := benchTask("EvactorPicklingBench", 20000 to 40000 by 2000).evaluated,
    evactor2Java       := benchTask("EvactorJavaBench", 20000 to 40000 by 2000).evaluated,
    evactor2Kryo       := benchTask("EvactorKryoBench", 20000 to 40000 by 2000).evaluated,
    sparklrPick        := benchTask("SparkLRPicklingBench", 20000 to 40000 by 2000).evaluated,
    sparklrJava        := benchTask("SparkLRJavaBench", 20000 to 40000 by 2000).evaluated,
    sparklrKryo        := benchTask("SparkLRKryoBench", 20000 to 40000 by 2000).evaluated,
    graphPick          := benchTask("WikiGraphPicklingBench", 5000 to 14000 by 1000).evaluated,
    graphJava          := benchTask("WikiGraphJavaBench", 5000 to 14000 by 1000).evaluated,
    graphKryo          := benchTask("WikiGraphKryoBench", 5000 to 14000 by 1000).evaluated
  )
}
