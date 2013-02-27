/*
  Shortest Test
*/
import scala.reflect.macros.{Context => Ctx}
import language.experimental.macros

object Test {
  def main(args: Array[String]): Unit = {
    println(A1.foo(42))
  }
}

// Bug: 'extends T1'
// Extension in Start Object produce
// false initialisation workflow
object A1 extends T1 {
  def impl(c: Ctx)(x: c.Expr[Any]) = {
    A2.A2_meth1
    x
  }
  def foo(x: Any) = macro A1.impl
}

trait T1 {
  def t1 = 1
}

trait T2 {
  def t1 = 1
}

object A2 extends T2 {
  def A2_meth1 = 1
}
