package scala.pickling.test.issue50

import scala.pickling._, scala.pickling.Defaults._, json._
import org.scalatest.FunSuite

case class TestA(x: Option[Int])
case class TestB(x: Option[String])
class Simple(val x: (String, Int)) {}

class Issue50Test extends FunSuite {
  test("Issue #50") {
    val a = TestA(Some(1))
    val pa = a.pickle
    assert(pa.unpickle[TestA] == a)

    val b = TestB(Some("hello"))
    val pb = b.pickle
    assert(pb.unpickle[TestB] == b)
  }
}

class Issue50StackOverflowTest extends FunSuite {
  test("Issue #50 SO http://stackoverflow.com/questions/19413038/unpickler-for-class-with-tuple") {
    val s = new Simple(("test", 3))
    val simplePickled = s.pickle
    assert(simplePickled.unpickle[Simple].x == s.x)
  }
}
