import scala.pickling._
import binary._
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

object Test extends App {

  val data = Map(1 -> ("Jim", 30), 2 -> ("Bart", 45))

  implicit def personp(implicit intp: Pickler[Int]): Pickler[Person] =
    new Pickler[Person] {
      val format = intp.format
      def pickle(p: Person, builder: PickleBuilder): Unit = {
        // let's say we only want to pickle id, since we can look up name and age based on id
        // then we can make use of a size hint, so that a fixed-size array can be used for pickling
        builder.hintTag(typeTag[Person])
        builder.hintKnownSize(14)
        builder.beginEntry(p)
        builder.putField("id", b => {
          b.hintTag(typeTag[Int])
          b.hintStaticallyElidedType()
          intp.pickle(p.id, b)
        })
        builder.endEntry()
      }
    }

  implicit def personup(implicit intup: Unpickler[Int]): Unpickler[Person] =
    new Unpickler[Person] {
      val format = intup.format
      def unpickle(tag: TypeTag[_], reader: PickleReader): Any = {
        reader.hintTag(typeTag[Int])
        reader.hintStaticallyElidedType()
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
  println(pickle.value.mkString("[", ",", "]"))

  val p = pickle.unpickle[Person]
  println(p)
}
