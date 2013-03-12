/*
  Overriden Test
*/
import scala.reflect.macros.{Context => Ctx}
import language.experimental.macros

object Test {
  def main(args: Array[String]): Unit = {
    println(A1.foo(1))
  }
}

class C {
  def x:Int = 1
}

object A1  {
  def impl(c: Ctx)(x: c.Expr[Int]) = {
    var k:C = B
    val t = k.x
    k = new C

    import c.universe._

    val body = Apply(Select(x.tree, newTermName("$plus")), List(Literal(Constant(t+k.x+B.x))))
    c.Expr[Int](body)

  }
  def foo(x: Int) = macro A1.impl
}

object B extends C {
  override def x:Int = 20
}
