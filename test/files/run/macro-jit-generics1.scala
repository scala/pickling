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
    A3.A3_A33.fun33_fun1(1,1)
    A3.A3_A34.fun34_fun1(1,List(1))
  }
}

object A3 {
  object A3_A33 {
    def fun33_fun1[T,C](v:T,m:C) = {
      val t = new A4.C44[A5.C55]
      t.func44_fun1
    }
  }
  object A3_A34 {
    def fun34_fun1[T,C](v:T,m:List[C]):C = {
      m.head
    }
  }
}

object A4 {
  class C44[T] {
    def func44_fun1:Int = {
      //val t = new A5.C55
      //t.func55_fun1
      val m = new A6.C66[A7.C77[Int]]
      m.func66_fun1
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

object A6 {
  class C66[T] {
    def func66_fun1 = {
      1
    }
  }
}

object A7 {
  class C77[T] {
    def func77_fun1 = {
      1
    }
  }
}