import scala.reflect.macros.{Context => Ctx}
import language.experimental.macros

object Test {
  def main(args: Array[String]): Unit = {
    println(A1.foo(42))
  }
}

 /*{
  println("ImplT")
}*/

object A1 {
  def foo(x: Any) = macro impl

  def impl(c: Ctx)(x: c.Expr[Any]) = {
    O1.f1
    x
  }
}

object O2 {
  class C1
  class C2
  trait T1
}
object O3 {
  trait T2[T] {
    def f2:Int
    def f1 = {
      f2
      1
    }
  }
}
object O4 {
  class C4
}

object O1 extends O2.C1 with O2.T1 with O3.T2[O4.C4] {
  def f2:Int = 1
}
