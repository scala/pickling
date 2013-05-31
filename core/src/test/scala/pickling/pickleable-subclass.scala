package scala.pickling.pickleable_.subclass

import org.scalatest.FunSuite
import scala.pickling._
import binary._

@pickleable class Person(val name: String)
case class Firefighter(override val name: String, salary: Int) extends Person(name)

class PickleableSubclassTest extends FunSuite {
  test("main") {
    val pickle = new Firefighter("joe", 30000).pickle
    assert(pickle.toString === "BinaryPickle([0,0,0,47,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,112,105,99,107,108,101,97,98,108,101,95,46,115,117,98,99,108,97,115,115,46,70,105,114,101,102,105,103,104,116,101,114,0,0,0,3,106,111,101,0,0,117,48,0,0,0,3,106,111,101])")
    val ff = pickle.unpickle[Person]
    assert(ff.toString === "Firefighter(joe,30000)")
  }
}
