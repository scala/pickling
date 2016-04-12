
import scala.pickling._, scala.pickling.Defaults._, binary._
import org.scalatest.FunSuite


case class MyClass[A](myString: String, a: A)

class MyClassPickler[A](implicit val format: PickleFormat, aTypeTag: FastTypeTag[A],
                                     aPickler: Pickler[A], aUnpickler: Unpickler[A])
  extends Pickler[MyClass[A]] with Unpickler[MyClass[A]] {

  private val stringUnpickler = implicitly[Unpickler[String]]

  def tag: FastTypeTag[MyClass[A]] = FastTypeTag[MyClass[A]]

  override def pickle(picklee: MyClass[A], builder: PBuilder) = {
    builder.beginEntry(picklee, tag)
    builder.putField("myString",
      b => b.beginEntry(picklee.myString, FastTypeTag.String).endEntry()
    )
    builder.putField("a",
      b => {
        aPickler.pickle(picklee.a, b)
      }
    )
    builder.endEntry()
  }

  override def unpickle(tagKey: String, reader: PReader): MyClass[A] = {
    // TODO - use unpickle entry, save a few lines of code.
    val tag = reader.beginEntry()
	  val myStringUnpickled = stringUnpickler.unpickle(tag, reader).asInstanceOf[String]
	  reader.endEntry()
    val aTag = reader.beginEntry()
    val aUnpickled = aUnpickler.unpickle(aTag, reader).asInstanceOf[A]
    reader.endEntry()
    MyClass(myStringUnpickled, aUnpickled)
  }

}


class CustomGenericPicklerTest extends FunSuite {

  implicit def myClassPickler[A: Pickler: Unpickler: FastTypeTag](implicit pf: PickleFormat) =
    new MyClassPickler

  test("main") {
    val inst = MyClass("test", 42)
    val pickle = inst.pickle
    val up = pickle.unpickle[MyClass[Int]]
    assert(up.toString === inst.toString)
  }
}
