import scala.pickling._
import json._

class Person(val name: String, val age: Int)

object testImpl {
  implicit def theImpl[T]: List[T] = {
    List[T]()
  }
}
object Test extends App {
  import testImpl._

  val p = new Person("Bob",83)
  //val pickle = p.pickle
  val pickler = implicitly[List[Person]]
  //println(pickle.value)
}
