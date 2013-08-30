package scala.pickling.binary.collection

import org.scalatest.FunSuite
import scala.pickling._
import binary._

case class Person(x: Int)

class BinaryCollectionTest extends FunSuite {
  test("Seq") {
    val p = Seq(1, 2, 3).pickle
    val up = p.unpickle[Seq[Int]]
    assert(up === Seq(1, 2, 3))
  }

  test("Array") {
    val p = Array(Person(1), Person(2), Person(3)).pickle
    val up = p.unpickle[Array[Person]]
    assert(up === Array(Person(1), Person(2), Person(3)))
  }
}
