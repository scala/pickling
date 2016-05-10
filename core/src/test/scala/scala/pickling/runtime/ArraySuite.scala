package scala.pickling.test.runtime.array

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

class ArraySuite extends FunSuite {

  test("primitive array") {
    def testIt(x: Any): Unit = {
      val p = x.pickle
      val up = p.unpickle[Any]
      val arr = up.asInstanceOf[Array[Int]]
      assert(arr.mkString(",") == "5,6")
    }

    val arr: Array[Int] = Array(5, 6)

    testIt(arr)
  }

  test("non-primitive array") {
    def testIt(x: Any): Unit = {
      val p = x.pickle
      val up = p.unpickle[Any]
      val arr = up.asInstanceOf[Array[(Int, Double)]]
      assert(arr.mkString(",") == "(5,0.5),(6,0.6)")
    }

    val arr: Array[(Int, Double)] = Array(5 -> 0.5d, 6 -> 0.6d)

    testIt(arr)
  }

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
