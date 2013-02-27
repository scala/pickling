/*
  Reify test
  TODO: test fail if A1 with another name as class exists
*/
import scala.reflect.macros.{Context => Ctx}
import language.experimental.macros

object Test {
  def main(args: Array[String]): Unit = {
    A1.foo(42)
    A1.foo2(43)
  }
}

object A1 {
  def impl(c: Ctx)(x: c.Expr[Any]) = {
    import c.universe._
    val a = 1
    val b = 2
    reify { println( 1 + O.O.k ) }
  }
  def foo(x: Any) = macro A1.impl

  def impl2(c: Ctx)(x: c.Expr[Int]) = {
    import c.universe._
    val a = 1
    val b = 2
    reify { println( x.splice + O.O.k ) }
  }
  def foo2(x: Int) = macro A1.impl2
}


object O {
  object O {
    def k = 1
  }
}
