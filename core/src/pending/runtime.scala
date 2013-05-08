import scala.pickling._
import scala.pickling.json._

class Person(val name: String, val age: Int)

object Test extends App {

  val p: Any = new Person("joe", 23)

  // the following is invoking the macro to generate a Pickler[Any],
  // because p has type Any.
  // the trick is that we should detect that we're pickling a Person
  // and switch to runtime picklers.
  val pickle = p.pickle

  println("hooray:\n" + pickle.value)

}
