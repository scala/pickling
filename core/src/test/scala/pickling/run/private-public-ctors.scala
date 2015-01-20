package scala.pickling.privatepublicctorstest

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

class C private(private var x: Int, private var y: String) {
  // another private ctor
  private def this(x: Int) = this(x, "bye")
  // a single public ctor
  def this() = this(5, "hello")

  override def equals(other: Any): Boolean =
    other.isInstanceOf[C] && other.asInstanceOf[C].x == x && other.asInstanceOf[C].y == y
}

class PrivatePublicCtorsTest extends FunSuite {
  test("main") {
    val c = new C
    val pickle: JSONPickle = c.pickle
    val newC = pickle.unpickle[C]
    assert(c == newC)
  }
}

class RuntimePrivatePublicCtorsTest extends FunSuite {
  test("main") {
    val c = new C
    val p: JSONPickle = (c: Any).pickle
    val up = p.unpickle[Any]
    assert(c == up)
  }
}
