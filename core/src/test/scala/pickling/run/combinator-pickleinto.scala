package scala.pickling.combinator.pickleinto

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, binary._
import scala.reflect.runtime.universe._

// CUSTOM PICKLERS
// Step 1: pickle only certain fields (not so interesting, because can be done using transient)
// Step 2: pass size hints (if we know size of the fields to be pickled)
// Step 3 (separate test): invoke generation macro to generate default pickler, and then use that to run custom
//                         reinitialization logic (that means we don't need a special readObject method thanks to
//                         the generation macro)
//
// use of custom picklers: more speed with private fields that can be re-initialized in some other way
//
// another idea: could use custom picklers to obtain picklers for value classes

class Person(val id: Int) {
  private var _name: String = _
  def name = _name
  private var _age: Int = _
  def age = _age
  def initDetails(data: Map[Int, (String, Int)]) {
    val details = data(id)
    _name = details._1
    _age  = details._2
  }
  override def toString =
    "Person(" + name + ", " + age + ")"
}

class CombinatorPickleIntoTest extends FunSuite {
  test("main") {
    val data = Map(1 -> ("Jim", 30), 2 -> ("Bart", 45))

    implicit def personp(implicit intp: Pickler[Int]): Pickler[Person] =
      new Pickler[Person] {
        def tag: FastTypeTag[Person] = implicitly[FastTypeTag[Person]]
        def pickle(p: Person, builder: PBuilder): Unit = {
          // let's say we only want to pickle id, since we can look up name and age based on id
          // then we can make use of a size hint, so that a fixed-size array can be used for pickling
          builder.hintKnownSize(100) // FIXME: if the value is too small, we can get java.lang.ArrayIndexOutOfBoundsException
          builder.beginEntry(p, implicitly[FastTypeTag[Person]])
          builder.putField("id", b => {
            b.hintElidedType(FastTypeTag.Int)
            intp.pickle(p.id, b)
          })
          builder.endEntry()
        }
      }

    implicit def personup(implicit intup: Unpickler[Int]): Unpickler[Person] =
      new Unpickler[Person] {
        def tag: FastTypeTag[Person] = implicitly[FastTypeTag[Person]]
        def unpickle(tag: String, reader: PReader): Any = {
          reader.hintElidedType(FastTypeTag.Int)
          val tag = reader.beginEntry()
          val unpickled = intup.unpickle(tag, reader).asInstanceOf[Int]
          reader.endEntry()
          val p = new Person(unpickled)
          p.initDetails(data)
          p
        }
      }

    val bart = new Person(2)
    val pickle = bart.pickle
    val expected0 = "[0,0,0,43,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,99,111,109,98,105,110,97,116,111,114,46,112,105,99,107,108,101,105,110,116,111,46,80,101,114,115,111,110,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]"
    val expected = "[0,0,0,43,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,99,111,109,98,105,110,97,116,111,114,46,112,105,99,107,108,101,105,110,116,111,46,80,101,114,115,111,110,0,0,0,2]"
    assert(pickle.value.mkString("[", ",", "]") === expected0)

    val p = pickle.unpickle[Person]
    assert(p.toString === "Person(Bart, 45)")
  }
}
