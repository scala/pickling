package scala.pickling.binary.dpickler

import scala.language.existentials

import org.scalatest.FunSuite
import scala.pickling._, all._, binary._

abstract class Person(name: String, age: Int)
case class Firefighter(name: String, age: Int, salary: Int) extends Person(name, age)

class BinaryDPicklerTest extends FunSuite {
  test("main") {
    def letsDoIt()(implicit dp: DPickler[Person]): Unit = {
      val ff = Firefighter("Jim", 43, 30000)

      val builder = pickleFormat.createBuilder()
      dp.pickle(ff, builder)
      val pickle = builder.result()
      val up = pickle.unpickle[Person]

      assert(ff === up)
    }
    letsDoIt()
  }
}
