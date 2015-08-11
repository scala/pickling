package scala.pickling.non.public.weird

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

final class Person(private val name: String, age: Int, val hobby: Hobby) {
  // NOTE: be careful not to reference age anywhere, so that it's elided by the "constructors" phase
  override def toString = s"Person(name = $name, hobby = $hobby)"
}
final class Hobby(var name: String, private var notes: String, private val attitude: String) {
  override def toString = s"Hobby(name = $name, notes = $notes, attitude = $attitude)"
}

class NonPublicWeirdTest extends FunSuite {
  test("main") {
    val person = new Person("Eugene", 25, new Hobby("hacking", "mostly Scala", "loving it"))
    val personPickle = person.pickle
    System.err.println(personPickle.toString)
    assert(personPickle.toString === """
      |JSONPickle({
      |  "$type": "scala.pickling.non.public.weird.Person",
      |  "hobby": {
      |    "attitude": "loving it",
      |    "name": "hacking",
      |    "notes": "mostly Scala"
      |  },
      |  "name": "Eugene"
      |})
    """.stripMargin.trim)
    assert(personPickle.unpickle[Person].toString === person.toString)

    // TODO: when generating a Person pickler at runtime, we should be able to reuse the one which was generated at compile-time
    // but we aren't auto-registering the compiled picklers.
    // Once we do so these JSON pickles should change.
    val anyPickle = (person: Any).pickle
    assert(anyPickle.toString === """
      |JSONPickle({
      |  "$type": "scala.pickling.non.public.weird.Person",
      |  "name": "Eugene",
      |  "hobby": {
      |    "name": "hacking",
      |    "notes": "mostly Scala",
      |    "attitude": "loving it"
      |  }
      |})
    """.stripMargin.trim)
    assert(anyPickle.unpickle[Person].toString === person.toString)
  }
}
