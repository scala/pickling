package scala.pickling.methodinobject

import org.scalatest.FunSuite

object Test extends App {
    import scala.pickling._
    import json._
    def test(): String = {
        case class Person(name: String, age: Int) // case class defined in func test
        val res = Person("Hao", 10).pickle.value
        res
    }
    test
}

class MethodInObjectTest extends FunSuite {
  test("main") {
    assert(Test.test === """
      |{
      |  "tpe": "Person",
      |  "name": "Hao",
      |  "age": 10
      |}
    """.stripMargin.trim)
  }
}
