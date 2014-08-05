package scala.pickling.test.issue57

import scala.pickling._
import scala.pickling.json._

import org.scalatest.FunSuite

abstract class SimpleAbstract(val stringWrapper: SimpleProp) {}
class Simple(stringWrapper: SimpleProp) extends SimpleAbstract(stringWrapper) {}
case class SimpleProp(prop: String) {}

class Issue57Test extends FunSuite {
  test("Issue #57") {
    val simplePickle = new Simple(new SimpleProp("TestProp")).pickle
    val simpleUnpickle = simplePickle.unpickle[Simple]
    assert(simpleUnpickle.stringWrapper.toString === "SimpleProp(TestProp)")
  }
}
