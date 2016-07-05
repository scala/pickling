package scala.pickling.issues

import org.scalatest.FunSuite

import scala.pickling._
import scala.pickling.StringOutput
import Defaults._
import json._
import static._

class Issue385 extends FunSuite {

  /* It can't be an opened class hierarchy
     so we mark it case class to automate
     the pickler and unpickler generation */
  case class CluStream(
    dimC: Int,
    dimD: Int,
    nc: Int,
    nd: Int,
    threshold: Double,
    timeWindow: Double,
    lastN: Int,
    uniqueLabelCount: Int = 1
  )

  test("pickle/unpickle normally CluStream") {
    val c1 = CluStream(1,2,3,4,2.0,1.0,1)
    val pickled = c1.pickle
    val c2 = pickled.unpickle[CluStream]
    assert(c1 === c2)
  }

  test("use pickleTo with CluStream") {
    val c1 = CluStream(1,2,3,4,2.0,1.0,1)
    // Note that the implicit format is already
    // in scope because you have imported json
    val result1 = new StringOutput
    PickleOps(c1).pickleTo(result1)
    val result2 = c1.pickle.value
    assert(result1.result() === result2.trim)
  }

}
