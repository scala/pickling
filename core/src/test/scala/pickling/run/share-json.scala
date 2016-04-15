package scala.pickling.share.json

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

class C(val name: String, val desc: String, var c: C, val arr: Array[Int])

class ShareJsonTest extends FunSuite {

  import scala.pickling.internal.currentRuntime

  val c1 = new C("c1", "desc", null, Array(1))
  val c2 = new C("c2", "desc", c1, Array(1))
  val c3 = new C("c3", "desc", c2, Array(1))

  test("loop-share-nonprimitives") {

    currentRuntime.picklers.clearRegisteredPicklerUnpicklerFor[C]

    c1.c = c3
    val pickle = c1.pickle
    assert(pickle.toString === """
      |JSONPickle({
      |  "$type": "scala.pickling.share.json.C",
      |  "arr": [
      |    1
      |  ],
      |  "c": {
      |    "$type": "scala.pickling.share.json.C",
      |    "arr": [
      |      1
      |    ],
      |    "c": {
      |      "$type": "scala.pickling.share.json.C",
      |      "arr": [
      |        1
      |      ],
      |      "c": { "$ref": 0 },
      |      "desc": "desc",
      |      "name": "c2"
      |    },
      |    "desc": "desc",
      |    "name": "c3"
      |  },
      |  "desc": "desc",
      |  "name": "c1"
      |})
    """.trim.stripMargin)

    val c11 = pickle.unpickle[C]
    val c13 = c11.c
    val c12 = c13.c
    assert(c11.name === "c1")
    assert(c11.desc === "desc")
    assert(c11.arr.toList === List(1))
    assert(c12.name === "c2")
    assert(c12.desc === "desc")
    assert(c12.arr.toList === List(1))
    assert(c13.name === "c3")
    assert(c13.desc === "desc")
    assert(c13.arr.toList === List(1))
    assert(c12.c === c11)
  }

  test("loop-share-nothing") {

    currentRuntime.picklers.clearRegisteredPicklerUnpicklerFor[C]

    // Note we've been running out of memory on this test in jenkins,
    // which is also a legitimate success case
    try {
      import shareNothing._
      c1.c = c3
      c2.pickle
      fail("Expected a stack overflow or out of memory error")
    } catch {
      case x: StackOverflowError => ()
      case x: OutOfMemoryError => ()
    }
  }

  test("loop-share-everything") {

    currentRuntime.picklers.clearRegisteredPicklerUnpicklerFor[C]

    import shareEverything._
    c1.c = c3
    val pickle = c1.pickle
    assert(pickle.toString === """
      |JSONPickle({
      |  "$type": "scala.pickling.share.json.C",
      |  "arr": [
      |    1
      |  ],
      |  "c": {
      |    "$type": "scala.pickling.share.json.C",
      |    "arr": [
      |      1
      |    ],
      |    "c": {
      |      "$type": "scala.pickling.share.json.C",
      |      "arr": [
      |        1
      |      ],
      |      "c": { "$ref": 0 },
      |      "desc": "desc",
      |      "name": "c2"
      |    },
      |    "desc": "desc",
      |    "name": "c3"
      |  },
      |  "desc": "desc",
      |  "name": "c1"
      |})
    """.trim.stripMargin)

    val c11 = pickle.unpickle[C]
    val c13 = c11.c
    val c12 = c13.c
    assert(c11.name === "c1")
    assert(c11.desc === "desc")
    assert(c11.arr.toList === List(1))
    assert(c12.name === "c2")
    assert(c12.desc === "desc")
    assert(c12.arr.toList === List(1))
    assert(c13.name === "c3")
    assert(c13.desc === "desc")
    assert(c13.arr.toList === List(1))
    assert(c12.c === c11)
  }

  test("noloop-share-non-primitives") {

    currentRuntime.picklers.clearRegisteredPicklerUnpicklerFor[C]

    import shareNothing._
    c1.c = null
    val pickle = c3.pickle
    assert(pickle.toString === """
      |JSONPickle({
      |  "$type": "scala.pickling.share.json.C",
      |  "arr": [
      |    1
      |  ],
      |  "c": {
      |    "$type": "scala.pickling.share.json.C",
      |    "arr": [
      |      1
      |    ],
      |    "c": {
      |      "$type": "scala.pickling.share.json.C",
      |      "arr": [
      |        1
      |      ],
      |      "c": null,
      |      "desc": "desc",
      |      "name": "c1"
      |    },
      |    "desc": "desc",
      |    "name": "c2"
      |  },
      |  "desc": "desc",
      |  "name": "c3"
      |})
    """.trim.stripMargin)

    val c23 = pickle.unpickle[C]
    val c22 = c23.c
    val c21 = c22.c
    assert(c23.name === "c3")
    assert(c23.desc === "desc")
    assert(c23.arr.toList === List(1))
    assert(c22.name === "c2")
    assert(c22.desc === "desc")
    assert(c22.arr.toList === List(1))
    assert(c21.name === "c1")
    assert(c21.desc === "desc")
    assert(c21.arr.toList === List(1))
  }
}
