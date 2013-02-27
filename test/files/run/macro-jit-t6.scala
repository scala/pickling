/*
  Skip Test
*/
import scala.reflect.macros.{Context => Ctx}
import language.experimental.macros

object TTT
object Test {
  def main(args: Array[String]): Unit = {
    println(A1.foo(42))
  }
}

object A1 {
  def impl(c: Ctx)(x: c.Expr[Any]) = {
    A2.A2_meth1
    A3.jump

    A2.getClass.getMethods.find(_.getName == "A2_meth1").getOrElse(throw new Exception("A2_meth1 not Imported"))
    if (A2.getClass.getMethods.find(_.getName == "A2_meth2").isDefined)
      throw new Exception("A2_meth2 mistakely Imported")

    x
  }
  def foo(x: Any) = macro A1.impl
}

object A3 {
  def jump = {
    A2.A2_meth3
  }
}

object A2 {
  def A2_meth1 = 1
  def A2_meth2 = 2
  def A2_meth3 = 3
}
