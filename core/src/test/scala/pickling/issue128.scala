package scala.pickling.test.issue128

import scala.pickling._
import scala.pickling.json._

import org.scalatest.FunSuite

case class A(intOpt: Option[Int]) {
  intOpt match {
    case Some(int) =>
    case None =>
  }
}

class Issue128Test extends FunSuite {
  test("Issue #128") {
    val opt = Some(5)
    val a = A(opt)
    val pickle = a.pickle
    assert(pickle.unpickle[A] === a)
  }
}
