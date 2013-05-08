package scala.pickling.generics.simple

import org.scalatest.FunSuite
import scala.pickling._
import json._

case class C[T](x: T)

class GenericsSimpleTest extends FunSuite {
  test("main") {
    val c = C(2)
    val p = c.pickle
    assert(p.toString === """
      |JSONPickle({
      |  "tpe": "scala.pickling.genericssimple.C[scala.Int]",
      |  "x": 2
      |})
    """.trim.stripMargin)
    assert(p.unpickle[C[Int]] === c)
  }
}
