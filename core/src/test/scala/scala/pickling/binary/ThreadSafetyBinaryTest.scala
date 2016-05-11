package scala.pickling.`object`.graph.threadsafety.binary

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, binary._

class C(val name: String, val desc: String, var c: C, val arr: Array[Int])

class ThreadSafetyBinaryTest extends FunSuite {
  val c1 = new C("c1", "desc", null, Array(1))
  val c2 = new C("c2", "desc", c1, Array(1))
  val c3 = new C("c3", "desc", c2, Array(1))
  c1.c = c3

  test("object-graph-threadsafety") {
    val r = new Runnable {
      def run() = {
        for (_ <- 1 to 100) {
          val pickle = c1.pickle
          assert(pickle.toString === "BinaryPickle([0,0,0,49,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,111,98,106,101,99,116,46,103,114,97,112,104,46,116,104,114,101,97,100,115,97,102,101,116,121,46,98,105,110,97,114,121,46,67,0,0,0,1,1,0,0,0,0,0,0,49,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,111,98,106,101,99,116,46,103,114,97,112,104,46,116,104,114,101,97,100,115,97,102,101,116,121,46,98,105,110,97,114,121,46,67,0,0,0,1,1,0,0,0,0,0,0,49,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,111,98,106,101,99,116,46,103,114,97,112,104,46,116,104,114,101,97,100,115,97,102,101,116,121,46,98,105,110,97,114,121,46,67,0,0,0,1,1,0,0,0,-3,0,0,0,0,0,0,0,4,100,101,115,99,0,0,0,2,99,50,0,0,0,4,100,101,115,99,0,0,0,2,99,51,0,0,0,4,100,101,115,99,0,0,0,2,99,49])")

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
      }
    }

    val t1 = new Thread(r)
    val t2 = new Thread(r)
    t1.start
    t2.start
    t1.join
    t2.join
  }
}