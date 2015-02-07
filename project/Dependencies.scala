import sbt._
import Keys._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "2.1.7"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.11.6"
  lazy val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
  lazy val macroParadise = "org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full
  lazy val quasiquotes = "org.scalamacros" %% "quasiquotes" % "2.0.1"
  lazy val kryoSerializers = "de.javakaffee" % "kryo-serializers" % "0.22"
  lazy val kryo = "com.esotericsoftware.kryo" % "kryo" % "2.20"
}
