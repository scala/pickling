package scala.pickling

import scala.reflect.{classTag, ClassTag}
import scala.tools.reflect.{ToolBox, ToolBoxError}

object NegativeCompilation {

  // utilities for adding failed tests to scalatest
  implicit class objectops(obj: Any) {
    def mustBe(other: Any) = assert(obj == other, obj + " is not " + other)

    def mustEqual(other: Any) = mustBe(other)
  }

  implicit class stringops(text: String) {
    def mustContain(substring: String) = assert(text contains substring, text)

    def mustStartWith(prefix: String) = assert(text startsWith prefix, text)
  }

  implicit class listops(list: List[String]) {
    def mustStartWith(prefixes: List[String]) = {
      assert(list.length == prefixes.size, ("expected = " + prefixes.length + ", actual = " + list.length, list))
      list.zip(prefixes).foreach{ case (el, prefix) => el mustStartWith prefix }
    }
  }

  def intercept[T <: Throwable : ClassTag](body: => Any): T = {
    try {
      body
      throw new Exception(s"Exception of type ${classTag[T]} was not thrown")
    } catch {
      case t: Throwable =>
        if (classTag[T].runtimeClass != t.getClass) throw t
        else t.asInstanceOf[T]
    }
  }

  def eval(code: String, compileOptions: String = ""): Any = {
    // println(s"eval compile options: $compileOptions")
    val tb = mkToolbox(compileOptions)
    tb.eval(tb.parse(code))
  }

  def mkToolbox(compileOptions: String = ""): ToolBox[_ <: scala.reflect.api.Universe] = {
    val m = scala.reflect.runtime.currentMirror
    import scala.tools.reflect.ToolBox
    m.mkToolBox(options = compileOptions)
  }

  def scalaBinaryVersion: String = {
    val PreReleasePattern = """.*-(M|RC).*""".r
    val Pattern = """(\d+\.\d+)\..*""".r
    val SnapshotPattern = """(\d+\.\d+\.\d+)-\d+-\d+-.*""".r
    scala.util.Properties.versionNumberString match {
      case s @ PreReleasePattern(_) => s
      case SnapshotPattern(v) => v + "-SNAPSHOT"
      case Pattern(v) => v
      case _          => ""
    }
  }

  def toolboxClasspath = {
    val f0 = new java.io.File(s"core/target/scala-${scalaBinaryVersion}/classes")
    val f1 = new java.io.File(s"test-util/target/scala-${scalaBinaryVersion}/test-classes")
    val fs = Vector(f0, f1)
    fs foreach { f => if (!f.exists) sys.error(s"output directory ${f.getAbsolutePath} does not exist.") }
    val sep = sys.props("file.separator")
    fs.map(_.getAbsolutePath).mkString(sep)
  }

  def quasiquotesJar: String = {
    val dir = System.getProperty("user.dir")
    if (scalaBinaryVersion == "2.10") s":$dir/quasiquotes_2.10-2.0.1.jar"
    else ""
  }

  def expectError(errorSnippet: String, compileOptions: String = "",
                  baseCompileOptions: String = s"-cp ${toolboxClasspath}${quasiquotesJar}")(code: String) {
    intercept[ToolBoxError] {
      eval(code, compileOptions + " " + baseCompileOptions)
    }.getMessage mustContain errorSnippet
  }

}
