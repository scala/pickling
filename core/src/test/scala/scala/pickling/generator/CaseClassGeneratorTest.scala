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
}


final case class CaseClassNoConstructor()
final case class SimpleCaseClass(x: Int, y: String)
final case class PrivateConstructorCaseClass private (x: Int, y: String) {}
final case class MultipleParamListCaseClass(x: Int)(val y: String)
// TODO - Should we persist the y or not?
final case class NestedVarCaseClass(x: Int) { var y: Int = 0 }