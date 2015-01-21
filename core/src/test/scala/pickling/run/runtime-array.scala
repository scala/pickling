package scala.pickling.test.runtime.array

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

class RuntimeArrayTest extends FunSuite {

  test("primitive array") {
    def testIt(x: Any): Unit = {
      val p = x.pickle
      val up = p.unpickle[Any]
      val arr = up.asInstanceOf[Array[Int]]
      assert(arr.mkString(",") == "5,6")
    }

    val arr: Array[Int] =
      Array(5, 6)

    testIt(arr)
  }

  test("non-primitive array") {
    def testIt(x: Any): Unit = {
      val p = x.pickle
      val up = p.unpickle[Any]
      val arr = up.asInstanceOf[Array[(Int, Double)]]
      assert(arr.mkString(",") == "(5,0.5),(6,0.6)")
    }

    val arr: Array[(Int, Double)] =
      Array(5 -> 0.5d, 6 -> 0.6d)

    testIt(arr)
  }

}
