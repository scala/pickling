import scala.pickling._
import json._

case class Job(title: String)
case class Person(name: String, age: Int, job: Job)
case class Philipp(mother: Person)

object Test extends App {
  val gudrun = Person("gudrun", 62, Job("Teacher"))
  val pckl = Philipp(gudrun).pickle
  println(pckl.value)
  println(pckl.unpickle[Philipp])
}
