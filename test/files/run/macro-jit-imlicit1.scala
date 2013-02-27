import scala.reflect.macros.{Context => Ctx}
import language.experimental.macros

object Test {
  def main(args: Array[String]): Unit = {
    println(A1.foo(42))
  }
}

class ImplT

 /*{
  println("ImplT")
}*/

object A1 {

  def foo(x: Any) = macro impl

  implicit def implcV:ImplT = new ImplT
  val nonImplcV:ImplT = new ImplT
  val notype = new ImplT

  def impl(c: Ctx)(x: c.Expr[Any]) = {
    //val k:ImplT = new ImplT

    A2.A2_meth2(1)

    //notype
    x
  }
}

object A2 {
  def A2_meth2(a:Int)(implicit b:ImplT) = {
    a+1
  }
}