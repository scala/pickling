import scala.pickling._
import ir._
import json._
import scala.reflect.runtime.{universe => ru}
import ru._

case class Person(name: String, age: Int)

object Test extends App {
  val irs = new IRs(ru)
  val ir = ObjectIR(typeOf[Person], List(FieldIR("name", typeOf[String], StringIR("Bob")), FieldIR("age", typeOf[Int], IntIR(61))))
  val jsonPickle = JSONPickleFormat.write(ir)
  println(jsonPickle.value)
}