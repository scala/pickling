package scala.pickling.staticonlywithmanualpickler

import org.scalatest.FunSuite
import scala.pickling._, ops._, json._
import static.StaticOnly

// NOT sealed so StaticOnly should block generating
// a pickler.
class NotClosed(fld: Int)

case class FakeImplementation() extends Exception("Not a real implementation")

class StaticOnlyWithManualPicklerTest extends FunSuite {
  test("main") {
    val x: NotClosed = new NotClosed(42)
    // StaticOnly should be happy with us, because
    // we define this pickler. If we remove this, then
    // this file should not compile.
    implicit val picklerUnpickler: SPickler[NotClosed] with Unpickler[NotClosed] = new SPickler[NotClosed] with Unpickler[NotClosed] {
      def pickle(picklee: NotClosed, builder: PBuilder): Unit =
        throw FakeImplementation()
      def unpickle(tag: String, reader: PReader): Any =
        throw FakeImplementation()
      def tag = FastTypeTag[NotClosed]
    }
    val pickle: JSONPickle = try {
      x.pickle
      throw new AssertionError("Should have used the fake implementation pickler")
    } catch {
      case FakeImplementation() => JSONPickle("")
    }
    try {
      pickle.unpickle[NotClosed]
      throw new AssertionError("Should have used the fake implementation unpickler")
    } catch {
      case PicklingException(msg, cause) =>
        assert(msg.contains("failed to parse"))
    }
  }
}
