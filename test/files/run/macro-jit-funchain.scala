import scala.reflect.macros.{Context => Ctx}
import language.experimental.macros

object Test {
  def main(args: Array[String]): Unit = {
    println(A1.foo(42))
  }
}

object A1 {
  def foo(x: Any) = macro impl

  def impl(c: Ctx)(x: c.Expr[Any]) = {
    A2.a2_fun1
    x
  }
}

object A2 {
  def a2_fun1:Int = {
    A2.A22.a22_fun1
  }
  object A22 {
    def a22_fun1:Int = a2_fun2
  }
  def a2_fun2:Int = {
    // Direct call via non ininted elements chain
    A3.A3_A33.fun33_fun1
  }
}

object A3 {
  object A3_A33 {
    // Untyped func (check correct DefDef completion)
    def fun33_fun1 = {
      val t = new A4.C44
      t.func44_fun1
    }
  }
}

object A4 {
  class C44 {
    def func44_fun1:Int= {
      val t = new A5.C55
      t.func55_fun1
    }
  }
}

object A5 {
  class C55 {
    def func55_fun1 = {
      1
    }
  }
}
