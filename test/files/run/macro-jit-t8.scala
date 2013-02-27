/*
  Shortest Test
*/
import scala.reflect.macros.{Context => Ctx}
import language.experimental.macros

object Test {
  def main(args: Array[String]): Unit = {
    A1.foo(1, 2, 3, 4, 5)
  }
}

object A1 {
  def myprintln(xs: Int*) = {
    println(xs)
  }

  def impl(c: Ctx)(xs: c.Expr[Int]*) = {
    import c.universe._

    val t =  xs.map(_.tree)

    val body = Apply(Select(Ident(newTermName("A1")), newTermName("myprintln")), xs.map(_.tree).toList)
    c.Expr[Unit](body)
  }
  def foo(xs: Int*) = macro A1.impl
}

