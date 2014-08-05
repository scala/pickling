package scala.pickling.option.primitive

import org.scalatest.FunSuite
import scala.pickling._
import binary._

class OptionPrimitiveTest extends FunSuite {
  test("main") {
    val opt = Some(9)
    val pckl = opt.pickle
    assert(pckl.unpickle[Option[Int]] === opt)
  }
}
