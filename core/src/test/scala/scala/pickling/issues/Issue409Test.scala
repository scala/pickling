package scala.pickling.generation

import org.scalatest.FunSuite
import scala.pickling._, Defaults._, json._

class CheckGenPicklerUnpicklerTogether extends FunSuite {

  case class Pi(number: Int)

  test("check that pickler and unpickler are generated together (I)") {

    val p = implicitly[Pickler[Pi]]
    val u = implicitly[Unpickler[Pi]]

    assert(p === u)

  }

}
