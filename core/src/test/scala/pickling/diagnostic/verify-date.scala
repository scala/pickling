package scala.pickling.diagnostic.dates

import scala.pickling._
import scala.pickling.Defaults._
import org.scalatest.FunSuite

class DateVerifyTestSuite extends FunSuite {
  implicit val debugFormat = new scala.pickling.diagnostic.DiagnosticPickleFormat(scala.pickling.json.pickleFormat)

  // TODO - Use scalacheck for this
  test("Date(...).pickle abides by all the rules") {
    val d = new java.util.Date()
    d.pickle
  }

  // TODO - unpickle.
}
