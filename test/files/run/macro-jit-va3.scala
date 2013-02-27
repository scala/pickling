import scala.reflect.macros.{Context => Ctx}
import language.experimental.macros

// Implicit
// Class method/val access
class Cl1(a:Int) {
   def realfunc(v:Int) = v+1
   val b = a + 1
}

class Cl2(a:Int) {
   def realfunc(v:Int) = v+1
   val b = a + 1
}

class Cl3(a:Int) {
   def realfunc(v:Int) = v+1
   val b = a + 1
}

object A1 {
  def foo(x: Int) = macro impl
  def a1 = 0x8
  val a2 = 0x4
  def _a1 = 0x80
  val _a2 = 0x40
  val a3 = 0x200
  implicit def implc1(x:Cl1):Int = x.b
  def ext1 = {
    //val t = List[Cl2]()
    1
  }
  def impl(c: Ctx)(x: c.Expr[Int]) = {
    val t = A2.val1
    val t2 = A2.A22.val2
    val t3 = t.b
    val t4 = _a1
    val t5 = _a2
    val t6 = t.realfunc(a3)
    val t7 = A2.cl3.b
    // ValDef@16776(Modifiers(), newTermName("t7"), TypeTree[31](), Apply(TypeApply(Ident(newTermName("List")), List(Ident@6924[32](Cl2#6924)))
    // ValDef@16776(Modifiers(), newTermName("t7"), TypeTree(), Select@11005(This@4671(newTypeName("immutable")), scala.collection.immutable.Nil#11005))
    //val t7 = List[Cl2](new Cl2(1))
    ext1
    val s1 = A2._val1.b + A2.A22._val2 + _a1 + _a2 + A11.val1
    import c.universe._
    //A2.A2_meth2(1)(nonImplcV)
    val body = Apply(Select(x.tree, newTermName("$plus")), List(Literal(Constant(t+s1+t2+t3+t4+t5))))
    c.Expr[Int](body)
  }

  object A11 {
    val val1:Int = 0x100
  }
}

object A2 {
  object A22 {
    val val2 = 0x1
    val _val2 = 0x10
  }
  val val1:Cl1 = new Cl1(0x2)
  val _val1:Cl1 = new Cl1(0x20)
  val cl3:Cl3 = new Cl3(0x20)
}

object Test {
  def main(args: Array[String]): Unit = {
    printf("%08x",A1.foo(0))
  }
}
