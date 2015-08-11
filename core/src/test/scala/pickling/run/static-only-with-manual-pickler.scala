package scala.pickling.staticonlywithmanualpickler

import org.scalatest.FunSuite
import scala.pickling._, functions._, json._
import static.StaticOnly

// NOT sealed so StaticOnly should block generating
// a pickler.
class NotClosed(fld: Int)

final case class FakeImplementation() extends Exception("Not a real implementation")
final case class Apple(x: Int)

class StaticOnlyWithManualPicklerTest extends FunSuite {
  test("main") {
    val x: NotClosed = new NotClosed(42)
    // StaticOnly should be happy with us, because
    // we define this pickler. If we remove this, then
    // this file should not compile.
    implicit val picklerUnpickler: Pickler[NotClosed] with Unpickler[NotClosed] =
      new Pickler[NotClosed] with Unpickler[NotClosed] {
        def pickle(picklee: NotClosed, builder: PBuilder): Unit =
          throw FakeImplementation()
        def unpickle(tag: String, reader: PReader): Any =
          throw FakeImplementation()
        def tag = FastTypeTag[NotClosed]
      }
    val pkl: JSONPickle = try {
      pickle(x)
      throw new AssertionError("Should have used the fake implementation pickler")
    } catch {
      case FakeImplementation() => JSONPickle("")
    }
    try {
      unpickle[NotClosed](pkl)
      throw new AssertionError("Should have used the fake implementation unpickler")
    } catch {
      case PicklingException(msg, cause) =>
        assert(msg.contains("failed to parse"))
    }
  }

  // Test that you can generate Pickler without having ops._ imported on the callsite.
  test ("manually generated pickler") {
    import scala.pickling.Defaults.intPickler
    import scala.pickling.Defaults.refPickler
    import scala.pickling.Defaults.refUnpickler
    implicit val applePickler: Pickler[Apple] = Pickler.generate[Apple]
    implicit val appleUnpickler: Unpickler[Apple] = Unpickler.generate[Apple]
    val pkl: JSONPickle = pickle(Apple(1))
    val unpickled = unpickle[Apple](pkl)
    assert(Apple(1) == unpickled)
  }

  // Test that you can generate PicklerUnpickler and use it to both  pickle
  // and unpickle
  test ("manually generated picklerunpickler") {
    import scala.pickling.Defaults.intPickler
    import scala.pickling.Defaults.refPickler
    import scala.pickling.Defaults.refUnpickler
    implicit val applePicklerUnpickler = PicklerUnpickler.generate[Apple]
    val pkl: JSONPickle = pickle(Apple(1))
    val unpickled = unpickle[Apple](pkl)
    assert(Apple(1) == unpickled)
  }
}
