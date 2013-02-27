/*
  Shortest Test
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
    A2.A2_val1
    x
  }
  def foo(x: Any) = macro A1.impl
}

object A2 {
  val A2_val1 = 1
}
