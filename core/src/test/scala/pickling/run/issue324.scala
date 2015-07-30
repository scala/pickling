package scala.pickling.test.issue324

import org.scalatest.FunSuite
import scala.pickling.Defaults._, scala.pickling.json._

case class Foo(bar: Set[Int]) { val barSize = bar.size }

class Issue324 extends FunSuite {

  test("Issue #324") {
    val p = Foo(Set()).pickle
    val up = p.unpickle[Foo]
    assert(up.toString == "Foo(Set())")
  }

}
