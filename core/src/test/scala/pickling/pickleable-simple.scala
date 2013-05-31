package scala.pickling.pickleable_.simple

import org.scalatest.FunSuite
import scala.pickling._
import binary._

@pickleable case class Person(name: String)

class PickleableSimpleTest extends FunSuite {
  test("main") {
    val pickle = new Person("joe").pickle
    assert(pickle.toString === "BinaryPickle([0,0,0,40,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,112,105,99,107,108,101,97,98,108,101,95,46,115,105,109,112,108,101,46,80,101,114,115,111,110,0,0,0,3,106,111,101])")
    val ff = pickle.unpickle[Person]
    assert(ff.toString === "Person(joe)")
  }
}
