package scala.pickling.non.public.joint

import org.scalatest.FunSuite
import scala.pickling._
import json._

class Person(private val name: String, age: Int, val hobby: Hobby) {
  // NOTE: be careful not to reference age anywhere, so that it's elided by the "constructors" phase
  override def toString = s"Person(name = $name, hobby = $hobby)"
}
class Hobby(var name: String, private var notes: String, private val attitude: String) {
  override def toString = s"Hobby(name = $name, notes = $notes, attitude = $attitude)"
}

class NonPublicJointTest extends FunSuite {
  test("main") {
    val e = new Person("Eugene", 25, new Hobby("hacking", "mostly Scala", "loving it"))
    val pickle = e.pickle
    assert(pickle.toString === """
      |JSONPickle({
      |  "tpe": "scala.pickling.non.public.joint.Person",
      |  "name": "Eugene",
      |  "hobby": {
      |    "name": "hacking",
      |    "notes": "mostly Scala",
      |    "attitude": "loving it"
      |  }
      |})
    """.stripMargin.trim)
    assert(pickle.unpickle[Person].toString === e.toString)
  }
}
