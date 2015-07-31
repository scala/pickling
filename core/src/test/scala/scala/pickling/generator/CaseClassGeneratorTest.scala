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
    implicit val pu = scala.pickling.functions.testNewThing2[NestedValCaseClass]
    val x = NestedValCaseClass(5)
    val y = x.pickle.unpickle[NestedValCaseClass]
    assert(x == y)
  }
  test("protectedMember") {
    implicit val pu = scala.pickling.functions.testNewThing2[ProtectedMemberCaseClass]
    val x = ProtectedMemberCaseClass(5, "hi")
    val y = x.pickle.unpickle[ProtectedMemberCaseClass]
    assert(x == y)
  }
  // NOTE: Old pickling aglorithm fails on this case
  test("privateMember") {
    implicit val pu = scala.pickling.functions.testNewThing2[PrivateMemberCaseClass ]
    val x = PrivateMemberCaseClass(5, "hi")
    val y = x.pickle.unpickle[PrivateMemberCaseClass]
    assert(x == y)
  }
  test("nestedPrivateVal") {
    implicit val pu = scala.pickling.functions.testNewThing2[NestedPrivateVarCaseClass]
    val x = NestedPrivateVarCaseClass(1)
    val y = x.pickle.unpickle[NestedPrivateVarCaseClass]
    assert(x == y)
  }
  test("nestedPrivateThisVar") {
    implicit val pu = scala.pickling.functions.testNewThing2[NestedPrivateThisCaseClass]
    val x = NestedPrivateThisCaseClass(1)
    val y = x.pickle.unpickle[NestedPrivateThisCaseClass]
    assert(x.toString == y.toString)
  }
}

// Case 1 - empty
final case class CaseClassNoConstructor()
// Case 2 - simple
final case class SimpleCaseClass(x: Int, y: String)
// Case 3 - varags
final case class MultipleParamListCaseClass(x: Int)(val y: String)
// Case 4 - Nested public var
final case class NestedVarCaseClass(x: Int) { var y: Int = 0 }
// Case 5 - Embedded in class
final class NestedCaseClassHolder {
  final case class NestedCaseClass(x: Int)
  object NestedCaseClass {
    import scala.pickling.Defaults._
    implicit val p = scala.pickling.functions.testNewThing2[NestedCaseClass]
  }
}
// Case 6 - Nested public val
case class NestedValCaseClass(x: Int) {
  val y = "Hi"
}
// Case 7 - protected val
case class ProtectedMemberCaseClass(x: Int, protected val y: String)
// Case 8 - private val
case class PrivateMemberCaseClass(x: Int, private val y: String)

// Case 9 - private cosntructor (SCALA BUG PREVENTS THIS FROM COMPILING)
final case class PrivateConstructorCaseClass private (x: Int, y: String) {}

// Case 10 - nested private var
final case class NestedPrivateVarCaseClass(x: Int) {
  private var y = NestedPrivateVarCaseClass.globalY
  override def equals(x: Any): Boolean =
    x match {
      case null => false
      case x: NestedPrivateVarCaseClass => (x.y == y) && (x.x == x.x)
      case _ => false
    }
  override def toString = s"NestedPrivateVarCaseClass($x) { var y = $y }"
}
object NestedPrivateVarCaseClass {
  private var globalY = 1
  private def nextY = {
    globalY += 1
    globalY
  }
}

// Case 11 - nested private[this]
final case class NestedPrivateThisCaseClass(x: Int) {
  private[this] val y = NestedPrivateThisCaseClass.globalY
  override def toString = s"NestedPrivateThisCaseClass($x) { var y = $y }"
}
object NestedPrivateThisCaseClass {
  private var globalY = 1
  private def nextY = {
    globalY += 1
    globalY
  }
}