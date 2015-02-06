
import scala.pickling._, scala.pickling.Defaults._, binary._
import org.scalatest.FunSuite


case class MyClass[A](myString: String, a: A)

class MyClassPickler[A](implicit val format: PickleFormat, aTypeTag: FastTypeTag[A],
                                     aPickler: Pickler[A], aUnpickler: Unpickler[A])
  extends Pickler[MyClass[A]] with Unpickler[MyClass[A]] {

  private val stringUnpickler = implicitly[Unpickler[String]]

  def tag: FastTypeTag[MyClass[A]] = implicitly[FastTypeTag[MyClass[A]]]

  override def pickle(picklee: MyClass[A], builder: PBuilder) = {
    builder.beginEntry(picklee)

    builder.putField("myString",
      b => b.hintTag(FastTypeTag.String).beginEntry(picklee.myString).endEntry()
    )

    builder.putField("a",
      b => {
        b.hintTag(aTypeTag)
        aPickler.pickle(picklee.a, b)
      }
    )

    builder.endEntry()
  }

  override def unpickle(tagKey: String, reader: PReader): MyClass[A] = {
    reader.hintTag(FastTypeTag.String)
    val tag = reader.beginEntry()
	  val myStringUnpickled = stringUnpickler.unpickle(tag, reader).asInstanceOf[String]
	  reader.endEntry()

    reader.hintTag(aTypeTag)
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
