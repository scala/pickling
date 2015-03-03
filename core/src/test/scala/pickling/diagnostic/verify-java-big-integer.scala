package scala.pickling.debug.biginteger

import scala.pickling._
import scala.pickling.Defaults._
import org.scalatest.FunSuite
import java.math.BigInteger

class BigIntegerVerifyTestSuitediagnostic extends FunSuite {
  implicit val debugFormat = new scala.pickling.diagnostic.DiagnosticPickleFormat(scala.pickling.json.pickleFormat)

  // TODO - Use scalacheck for this
  test("BigInteger(...).pickle abides by all the rules") {
    new BigInteger("12345").pickle
  }

  // TODO - unpickle.
}
