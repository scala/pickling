package scala.pickling.base

import org.scalatest.FunSuite
import scala.pickling._
import scala.pickling.Defaults._
import json._

sealed abstract class Base
final class C extends Base { override def toString = "C" }
final class D extends Base { override def toString = "D" }

class BaseTest extends FunSuite {
  test("main") {
    val c: Base = new C
    val pc = c.pickle
    assert(pc.unpickle[Base].isInstanceOf[C] === true)

    val d: Base = new D
    val pd = d.pickle
    assert(pd.unpickle[Base].isInstanceOf[D] === true)
  }
}
