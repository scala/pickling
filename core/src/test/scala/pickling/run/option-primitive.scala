package scala.pickling.option.primitive

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, binary._

class OptionPrimitiveTest extends FunSuite {
  test("main") {
    val opt = Some(9)
    val pckl = opt.pickle
    assert(pckl.unpickle[Option[Int]] === opt)
  }
}
