package scala.pickling.singleton.simple

import org.scalatest.FunSuite
import scala.pickling._
import json._

object D {
  val shouldntSerializeMe = 42
}

class SingletonSimpleTest extends FunSuite {
  test("main") {
    val pickle = D.pickle
    assert(pickle.toString === """
      |JSONPickle({
      |  "tpe": "scala.pickling.singleton.simple.D.type"
      |})
    """.stripMargin.trim)
    assert((pickle.unpickle[D.type] eq D) === true)
  }
}
