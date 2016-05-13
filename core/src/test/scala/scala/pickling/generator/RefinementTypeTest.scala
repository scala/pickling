package scala.pickling

import org.scalatest.FunSuite
import Defaults._, json._, static._

class RefinementTypeTest extends FunSuite {

  sealed trait Refined {
    type T
    val data: T
  }

  case class Foo(data: Foo#T) extends Refined {
    type T = (Int, String)
  }

  test("pickle refined types correctly") {

    val f: Refined {type T = (Int, String)} = Foo(1 -> "hello")
    println(f.pickle.value)
    assert(false)

  }

}
