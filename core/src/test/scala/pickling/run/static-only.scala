package scala.pickling.staticonly

import org.scalatest.FunSuite
import scala.pickling._
import json._
import AllPicklers._
import static.StaticOnly

sealed trait A

sealed trait B extends A

sealed abstract class C extends B { val fld: Int }

final class D extends C { val fld = 1 }

final class E extends C {
  val fld = 2
  def incr(x: Int) = x + 1
}

// case class should work whether final or not
final case class F(bar: Int) extends A
case class G(bar: Int) extends A

// case class should work when extending B or C too
final case class H(bar: Int) extends B
final case class I(fld: Int) extends C

// companion object should not matter
case class J(baz: Long) extends B
object J {
  def apply(s: String): J = J(Integer.parseInt(s))
}

class StaticOnlyTest extends FunSuite {
  test("main") {
    val x: C = new D
    val pickle: JSONPickle = x.pickle
    assert(pickle.unpickle[C].fld == 1)
  }
  test("static-methods") {
  	val x: C = new D
    val pickle: JSONPickle = scala.pickling.pickle(x)
    assert(unpickle[C](pickle).fld == 1)
  }
}
