package scala.pickling.test.issue180.abstractsuper

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

trait TheTrait {
  val name : String
}

sealed abstract class BaseClass extends TheTrait {
  val name = "TestName"
}

case class TheClass(value : Int) extends BaseClass

class TraitWithAbstractValueTest extends FunSuite {

  test("main") {
    val instance = TheClass(1)
    val p = instance.pickle
    val up = p.unpickle[TheClass]
    assert(instance === up)
  }
}