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

object A1 {
  def impl(c: Ctx)(x: c.Expr[Any]) = {
    A2.A2_meth1
    x
  }
  def foo(x: Any) = macro A1.impl
}

object A2 extends NonUsedTrait1 with NN.NonUsedTrait2  with A3 {
  override def A2_meth1 = A3_meth1
  override def A3_meth2 = 10
  //def F5_fun1 = 1
  //def A2_meth1 = 1
}

trait NonUsedTrait1

object NN {
  trait NonUsedTrait2
}

trait A3 {
   def A3_meth1 = 1
   def A3_meth2 = 1

   def A2_meth1 = 1
}

trait D4 extends F5 {

  def A2_meth1 = 1

  def D4_fun1 = 1
  override def F5_fun1 = 1
}

trait F5 {
  def F5_fun1 = 1
  def F5_fun2 = 1
}