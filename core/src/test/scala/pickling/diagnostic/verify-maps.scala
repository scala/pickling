package scala.pickling.diagnostic.arrays

import scala.pickling._
import scala.pickling.Defaults._
import org.scalatest.FunSuite

class MapVerifyTestSuite extends FunSuite {
  implicit val debugFormat = new scala.pickling.diagnostic.DiagnosticPickleFormat(scala.pickling.json.pickleFormat)

  // TODO - Use scalacheck for this
  test("Map(...).pickle abides by all the rules") {
    Map(1->2, 2->3).pickle
    Map("1" -> 2).pickle
    Map("1" -> "2").pickle
    // TODO - object/object maps...
  }

  // TODO - unpickle.
}
