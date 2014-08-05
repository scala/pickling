package scala.pickling.runtime

import org.scalatest.FunSuite
import scala.pickling._
import json._

class Person(val name: String, val age: Int)

class RuntimeTest extends FunSuite {
  test("main") {
    val p: Any = new Person("joe", 23)

    // the following is invoking the macro to generate a Pickler[Any],
    // because p has type Any.
    // the trick is that we should detect that we're pickling a Person
    // and switch to runtime picklers.
    val pickle = p.pickle

    assert(pickle.value === """
      |{
      |  "$type": "scala.pickling.runtime.Person",
      |  "name": "joe",
      |  "age": 23
      |}
    """.stripMargin.trim)
  }
}
