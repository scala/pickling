package scala.pickling.generator

import org.scalatest.FunSuite

/**
 * Tests the case class generator
 */
class CaseClassGeneratorTest extends FunSuite {
  import scala.pickling.Defaults._
  import scala.pickling.json._
  test("noConstructor") {
    implicit val pu = scala.pickling.functions.testNewThing2[CaseClassNoConstructor]
    val x = CaseClassNoConstructor()
    val y = x.pickle.unpickle[CaseClassNoConstructor]
    assert(x == y)
  }

  test("simple") {
    implicit val pu = scala.pickling.functions.testNewThing2[SimpleCaseClass]
    val x = SimpleCaseClass(1, "hi")
    val y = x.pickle.unpickle[SimpleCaseClass]
    assert(x == y)
  }

  /* NOTE - This will not work due to https://issues.scala-lang.org/browse/SI-9425
  test("privateConstructor") {
    val x = PrivateConstructorCaseClass(1, "hi")
    implicit val pu = scala.pickling.functions.testNewThing2[PrivateConstructorCaseClass]
    val y = x.pickle.unpickle[PrivateConstructorCaseClass]
    assert(x == y)
  }
  */

  test("mulitpleParamList") {
    implicit val pu = scala.pickling.functions.testNewThing2[MultipleParamListCaseClass]
    val x = MultipleParamListCaseClass(1)("hi")
    val y = x.pickle.unpickle[MultipleParamListCaseClass]
    assert(x == y)
    assert(x.y == y.y)
  }

  test("nestedVar") {
    implicit val pu = scala.pickling.functions.testNewThing2[NestedVarCaseClass]
    val x = NestedVarCaseClass(1)
    x.y = 2
    val y = x.pickle.unpickle[NestedVarCaseClass]
    assert(x == y)
    assert(x.y == y.y)
  }

  test("nestedCaseClass") {
    val outer = new NestedCaseClassHolder()
    val x = outer.NestedCaseClass(5)
    val y = x.pickle.unpickle[outer.NestedCaseClass]
    assert(x == y)
  }
  test("nestedValCaseClass") {
    val x = NestedValCaseClass(5)
    val y = x.pickle.unpickle[NestedValCaseClass]
    assert(x == y)
  }
  test("protectedMember") {
    val x = ProtectedMemberCaseClass(5, "hi")
    val y = x.pickle.unpickle[ProtectedMemberCaseClass]
    assert(x == y)
  }
  test("privateMember") {
    val x = PrivateMemberCaseClass(5, "hi")
    val y = x.pickle.unpickle[PrivateMemberCaseClass]
    assert(x == y)
  }
}


final case class CaseClassNoConstructor()
final case class SimpleCaseClass(x: Int, y: String)
final case class MultipleParamListCaseClass(x: Int)(val y: String)
final case class NestedVarCaseClass(x: Int) { var y: Int = 0 }
final class NestedCaseClassHolder {
  final case class NestedCaseClass(x: Int)
  object NestedCaseClass {
    import scala.pickling.Defaults._
    implicit val p = scala.pickling.functions.testNewThing2[NestedCaseClass]
  }
}
case class NestedValCaseClass(x: Int) {
  val y = "Hi"
}
case class ProtectedMemberCaseClass(x: Int, protected val y: String)
case class PrivateMemberCaseClass(x: Int, private val y: String)
// TODOs

final case class PrivateConstructorCaseClass private (x: Int, y: String) {}
