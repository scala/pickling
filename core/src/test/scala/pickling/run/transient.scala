package scala.pickling.transienttest

import org.scalatest.FunSuite
import scala.pickling._
import json._

case class Person(val name: String , @transient val ssNumber: Int) {
  override def toString = s"Person($name)"
}

class TransientSimpleTest extends FunSuite {
  test("main") {
    val per = Person("Jenny", 123)
    val p: JSONPickle = per.pickle
    val up = p.unpickle[Person]
    assert(up.ssNumber == 0)
    assert(per.toString == up.toString)
  }
}