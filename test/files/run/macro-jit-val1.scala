// Val + Implicits bug

import scala.reflect.macros.{Context => Ctx}
import language.experimental.macros

class Cl1(a:Int) {
   def realfunc(v:Int) = v+1
   val b = a + 1
}

class Cl2(a:Int) {
   def realfunc(v:Int) = v+1
   val b = a + 1
}

object A1 {

  def foo(x: Int) = macro impl
  def a1 = 0x8
  val a2 = 0x4
  def _a1 = 0x80
  val _a2 = 0x40
  lazy val a3 = 0x200
  implicit def implc1(x:Cl1):Int = x.b
  def ext1 = {
    // ommit Nil typer conversion
    val t = List[Cl2]()
    val t1 = List[Al1](new Al2,new Al2)
    1
  }
  def impl(c: Ctx)(x: c.Expr[Int]) = {
    _a2
    import c.universe._
    ext1
    val body = A2.val1+1
    x
  }

  object A11 {
    val val1:Int = 0x100
  }
}

object A2 {
  val val1:Cl1 = new Cl1(0x2)
  val _val1:Cl1 = new Cl1(0x20)
}


object Test {
  def main(args: Array[String]): Unit = {
    printf("%08x",A1.foo(0))
  }
}

class Al1
class Al2 extends Al1
