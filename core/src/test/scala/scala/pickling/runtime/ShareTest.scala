package scala.pickling.share.binary.any

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, binary._

class C(val name: String, val desc: String, var c: C, val arr: Array[Int])

class ShareBinaryAnyTest extends FunSuite {

  import scala.pickling.internal.currentRuntime

  val c1 = new C("c1", "desc", null, Array(1))
  val c2 = new C("c2", "desc", c1, Array(1))
  val c3 = new C("c3", "desc", c2, Array(1))

  /*test("loop-share-nonprimitives") {
    c1.c = c3
    val pickle = (c1: Any).pickle
    assert(pickle.toString === "BinaryPickle([0,0,0,33,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,115,104,97,114,101,46,98,105,110,97,114,121,46,97,110,121,46,67,0,0,0,2,99,49,0,0,0,4,100,101,115,99,0,0,0,1,1,0,0,0,-1,0,0,0,2,99,51,0,0,0,4,100,101,115,99,0,0,0,1,1,0,0,0,-1,0,0,0,2,99,50,0,0,0,4,100,101,115,99,0,0,0,1,1,0,0,0,-1,0,0,0,2,99,49,0,0,0,4,100,101,115,99,0,0,0,1,1,0,0,0,-3,0,0,0,0])")

    val c11 = pickle.unpickle[Any].asInstanceOf[C]
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
  }*/

  test("loop-share-nothing") {

    currentRuntime.picklers.clearRegisteredPicklerUnpicklerFor[Any]
    currentRuntime.picklers.clearRegisteredPicklerUnpicklerFor[C]

    intercept[StackOverflowError] {
      import shareNothing._
      implicit val pu = PicklerUnpickler.generate[C]
      c1.c = c3
      (c2: Any).pickle
    }
  }

  /*test("loop-share-everything") {
    import shareEverything._
    c1.c = c3
    val pickle = (c1: Any).pickle
    assert(pickle.toString === "BinaryPickle([0,0,0,33,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,115,104,97,114,101,46,98,105,110,97,114,121,46,97,110,121,46,67,0,0,0,2,99,49,0,0,0,4,100,101,115,99,0,0,0,1,1,0,0,0,-1,0,0,0,2,99,51,-3,0,0,0,1,0,0,0,1,1,0,0,0,-1,0,0,0,2,99,50,-3,0,0,0,1,0,0,0,1,1,0,0,0,-1,-3,0,0,0,0,-3,0,0,0,1,0,0,0,1,1,0,0,0,-3,0,0,0,2])")
    val c11 = pickle.unpickle[Any].asInstanceOf[C]
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
  }*/

  test("noloop-share-non-primitives") {

    currentRuntime.picklers.clearRegisteredPicklerUnpicklerFor[C]

    import shareNothing._
    c1.c = null
    val pickle = (c3: Any).pickle
    assert(pickle.toString === "BinaryPickle([0,0,0,33,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,115,104,97,114,101,46,98,105,110,97,114,121,46,97,110,121,46,67,0,0,0,2,99,51,0,0,0,4,100,101,115,99,0,0,0,33,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,115,104,97,114,101,46,98,105,110,97,114,121,46,97,110,121,46,67,0,0,0,2,99,50,0,0,0,4,100,101,115,99,0,0,0,33,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,115,104,97,114,101,46,98,105,110,97,114,121,46,97,110,121,46,67,0,0,0,2,99,49,0,0,0,4,100,101,115,99,-2,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0])")
    val c23 = pickle.unpickle[Any].asInstanceOf[C]
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
