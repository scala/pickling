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

object A1 extends T1 with ExclusivlyForA1 {
  object A10 {
    def f5 = 1
  }
  object A11 {
    object A11 {
      def f9 = 1
    }
    def f8 = 1
  }
  val o3 = O3

  def foo(x: Any) = macro impl

  def impl(c: Ctx)(x: c.Expr[Any]) = {
    //0147 │SelectExternalsMarker : Select@6531(Select@6523(This@6520(newTypeName("A1")), A1.A10), newTermName("f5"))
    //0149 │qual.symbol=object A10#6523 el.symbol=method f5#6531 el.symbol.owner=object A10#6524 el.symbol.owner.companionSymbol=object A10#6523
    A10.f5
    o3.O33.f10
    A11.A11.f9
    A1.A10.f5
    O2.O22.f7
    // 0107 │ Select@7638(Ident@7634(O1), newTermName("f1"))
    // 0109 │qual.symbol=object O1#7634 el.symbol=method f1#7638 el.symbol.owner=trait T2#7636
    O1.f1
    // 0111 │ Select@7640(Ident@7634(O1), newTermName("f2"))
    // 0113 │qual.symbol=object O1#7634 el.symbol=method f2#7640 el.symbol.owner=object O1#7635
    O1.f2
    // 0118 │ Select@6530(This@6526(newTypeName("A1")), newTermName("f3"))
    // 0120 │qual.symbol=object A1#6526 el.symbol=method f3#6530 el.symbol.owner=object A1#6526
    f3
    // 0125 │ Select@7644(This@6526(newTypeName("A1")), newTermName("f4"))
    // 0127 │qual.symbol=object A1#6526 el.symbol=method f4#7644 el.symbol.owner=trait T1#6527
    f4

    x
  }
  def f3:Int = 1
}

trait ExclusivlyForA1 {

}

trait ExclusivlyForO1 {

}

trait T1 {
  def f4:Int = 1
}

trait T2 {
  def f1:Int = 1
}

object O1 extends ExclusivlyForO1 with T2 {
  def f2:Int = 1
}

object O2 {
  object O22 {
    def f7=1
  }
  def f6:Int = 1
}

object O3 {
  object O33 {
    def f10=1
  }
  def f11:Int = 1
}
