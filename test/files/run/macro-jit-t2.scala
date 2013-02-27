import scala.reflect.macros.{Context => Ctx}
import language.experimental.macros

class HELLO(a:Int) {
  def HELLO1 = 1
  def HELLO2 = 1
}

object Test {
  def main(args: Array[String]): Unit = {
    println(A1.foo(42))
  }
}

object A1 {
  self: Any =>

  def A1_meth1 = 1
  def A1_meth2 = 2
  def A1_meth3(x:Any) = x

  def impl(c: Ctx)(x: c.Expr[Any]) = {
    A2.A2_meth2
    x
  }

  def foo(x: Any) = macro A12.impl

  object A12 {
    def A12_meth1 = 1
    def A12_meth2 = {
      class local_A extends A2.A2_trait {
        def a = 1
      }
      val x = 1
      val y = new local_A
      A13.A13_meth1
      A12_meth4
    }
    def A12_meth3(x:Any) = x
    def A12_meth4 = 1
    def impl(c: Ctx)(x: c.Expr[Any]) = {
      A2.A2_meth2
      x
    }
  }

  object A13 {
    def A13_meth1 = 1
    def A13_meth2:Int = {
      A13_meth4
      A2.A2_meth2
      20
    }

    def A13_meth3(x:Int):Int = {
      A13_meth1
      x + 10
    }
    def A13_meth4 = 1
  }

}

object A2 {
  trait  A2_trait {
    def A2_traitA=1
  }
  object A2_inobj {
  }
  class MIROMAX
  type OUTERTYPE1 = Int

  def A2_meth2 = {
    val extTypeTest:OUTERTYPE1 = 1
    class intClassName extends MIROMAX
    case class caseClassInFunc(param1:Int)
    val intTypeTest:intClassName = new intClassName
    A2_meth3(10)
  }
  def A2_meth3(x:Any) = x
  object A22 {
    object A222 {
      def A222_meth3(c: Ctx)(x: c.Expr[Any]) = {
        val m = A1.A12.A12_meth2
        x
      }
    }
    def A22_meth1 = 1
    def A22_meth2 = 2
    def A22_meth3(c: Ctx)(x: c.Expr[Any]) = x
  }
}
