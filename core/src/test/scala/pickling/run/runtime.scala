package scala.pickling.runtime

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

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

class RuntimeArrayTests extends FunSuite {
  test("array-double") {
    val p: Any = Array(1.0, 2.0, 3.0)
    val pickle = p.pickle
    assert(pickle.value === """
      |{
      |  "$type": "scala.Array[scala.Double]",
      |  "value": [
      |    1.0,
      |    2.0,
      |    3.0
      |  ]
      |}
    """.stripMargin.trim)

    val up = pickle.unpickle[Any]
    assert(p === up)
  }

  test("array-int") {
    val p: Any = Array(1, 2, 3)
    val pickle = p.pickle
    assert(pickle.value === """
      |{
      |  "$type": "scala.Array[scala.Int]",
      |  "value": [
      |    1,
      |    2,
      |    3
      |  ]
      |}
    """.stripMargin.trim)

    val up = pickle.unpickle[Any]
    assert(p === up)
  }

  test("array-long") {
    val p: Any = Array(1L, 2L, 3L)
    val pickle = p.pickle
    assert(pickle.value === """
      |{
      |  "$type": "scala.Array[scala.Long]",
      |  "value": [
      |    "1",
      |    "2",
      |    "3"
      |  ]
      |}
    """.stripMargin.trim)

    val up = pickle.unpickle[Any]
    assert(p === up)
  }

  test("array-boolean") {
    val p: Any = Array(true, false, false)
    val pickle = p.pickle
    assert(pickle.value === """
      |{
      |  "$type": "scala.Array[scala.Boolean]",
      |  "value": [
      |    true,
      |    false,
      |    false
      |  ]
      |}
    """.stripMargin.trim)

    val up = pickle.unpickle[Any]
    assert(p === up)
  }

  test("array-short") {
    val one: Short = 1
    val two: Short = 2
    val three: Short = 3
    val p: Any = Array(one, two, three)
    val pickle = p.pickle
    assert(pickle.value === """
      |{
      |  "$type": "scala.Array[scala.Short]",
      |  "value": [
      |    1,
      |    2,
      |    3
      |  ]
      |}
    """.stripMargin.trim)

    val up = pickle.unpickle[Any]
    assert(p === up)
  }

  test("array-char") {
    val p: Any = Array('A', 'B', 'C')
    val pickle = p.pickle
    assert(pickle.value === """
      |{
      |  "$type": "scala.Array[scala.Char]",
      |  "value": [
      |    "A",
      |    "B",
      |    "C"
      |  ]
      |}
    """.stripMargin.trim)

    val up = pickle.unpickle[Any]
    assert(p === up)
  }

  test("array-byte") {
    val one: Byte = 1
    val two: Byte = 2
    val three: Byte = 3
    val p: Any = Array(one, two, three)
    val pickle = p.pickle
    assert(pickle.value === """
      |{
      |  "$type": "scala.Array[scala.Byte]",
      |  "value": [
      |    1,
      |    2,
      |    3
      |  ]
      |}
    """.stripMargin.trim)

    val up = pickle.unpickle[Any]
    assert(p === up)
  }

  test("array-float") {
    val p: Any = Array(1F, 2F, 3F)
    val pickle = p.pickle
    assert(pickle.value === """
      |{
      |  "$type": "scala.Array[scala.Float]",
      |  "value": [
      |    1.0,
      |    2.0,
      |    3.0
      |  ]
      |}
    """.stripMargin.trim)

    val up = pickle.unpickle[Any]
    assert(p === up)
  }

  test("array-string") {
    val p: Any = Array("one", "two", "three")
    val pickle = p.pickle
    assert(pickle.value === """
      |{
      |  "$type": "scala.Array[java.lang.String]",
      |  "elems": [
      |    {
      |    "$type": "java.lang.String",
      |    "value": "one"
      |  },
      |    {
      |    "$type": "java.lang.String",
      |    "value": "two"
      |  },
      |    {
      |    "$type": "java.lang.String",
      |    "value": "three"
      |  }
      |  ]
      |}
    """.stripMargin.trim)

    val up = pickle.unpickle[Any]
    assert(p === up)
  }
}

