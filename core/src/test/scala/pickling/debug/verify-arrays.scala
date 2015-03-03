package scala.pickling.debug.arrays

import scala.pickling._
import scala.pickling.Defaults._
import org.scalatest.FunSuite

class ArrayVerifyTestSuite extends FunSuite {
  implicit val debugFormat = new scala.pickling.debug.DebugPickleFormat(scala.pickling.json.pickleFormat)

  // TODO - Use scalacheck for this
  test("Array(...).pickle abides by all the rules") {
    Array(1,2,3).pickle
    Array(2L,3L).pickle
    Array(3f, 4f).pickle
    Array.empty[Double].pickle
    Array(1.0, 2.0).pickle
    Array(true, false).pickle
    Array('1', '2').pickle
    // Strings fail for now because we don't recongize them as primtiives...
    //Array("1", "2").pickle
  }

  // TODO - unpickle.
}
