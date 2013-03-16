import scala.pickling._
import json._
import scala.reflect.runtime.{universe => ru}

class Person(name: String, age: Int)

object Test extends App {

  val pf = new JSONPickleFormat

  val partial  = pf.putType(ru.typeOf[Person])

  val partial2 = pf.putPrimitive(partial, null, ru.typeOf[String], "name", "Gudrun")
  val partial3 = pf.putPrimitive(partial2, null, ru.typeOf[Int], "age", 62)

  val pickle   = pf.putObjectSuffix(partial3, null)

  println(pickle.value)
}
