package scala.pickling.staticonlywithmanualpickler

import org.scalatest.FunSuite
import scala.pickling._, functions._, json._
import static.StaticOnly

// NOT sealed so StaticOnly should block generating
// a pickler.
class NotClosed(fld: Int)

case class FakeImplementation() extends Exception("Not a real implementation")
case class Apple(x: Int)

class StaticOnlyWithManualPicklerTest extends FunSuite {
  test("main") {
    val x: NotClosed = new NotClosed(42)
    // StaticOnly should be happy with us, because
    // we define this pickler. If we remove this, then
    // this file should not compile.
    implicit val picklerUnpickler: SPickler[NotClosed] with Unpickler[NotClosed] =
      new SPickler[NotClosed] with Unpickler[NotClosed] {
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

  // Test that you can generate SPickler without having ops._ imported on the callsite.
  test ("manually generated pickler") {
    import scala.pickling.allPicklers.intPickler
    implicit val applePickler: SPickler[Apple] = SPickler.generate[Apple]
    implicit val appleUnpciker: Unpickler[Apple] = Unpickler.generate[Apple]
    val pkl: JSONPickle = pickle(Apple(1))
  }
}
