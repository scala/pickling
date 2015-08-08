package scala.pickling.generics.tough

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, binary._

class C[T]
case class D[T](x: T) extends C[T]

class GenericsToughTest extends FunSuite {
  test("main") {
    val c: C[Int] = D(2)
    val p = c.pickle
    // TODO - We shouldn't really force a binary format like this.
    //assert(p.toString === "BinaryPickle([0,0,0,42,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,103,101,110,101,114,105,99,115,46,116,111,117,103,104,46,68,91,115,99,97,108,97,46,73,110,116,93,0,0,0,2])")
    assert(p.unpickle[C[Int]] === c)
  }
}
