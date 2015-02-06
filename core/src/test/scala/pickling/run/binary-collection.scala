package scala.pickling.test.binary.collection

import scala.pickling._, scala.pickling.Defaults._, binary._
import org.scalatest.FunSuite

import scala.collection.mutable
import scala.collection.immutable
import scala.collection.IndexedSeq
import scala.collection.LinearSeq

case class Person(x: Int)

class BinaryCollectionTest extends FunSuite {
  test("Seq") {
    val p = Seq(1, 2, 3).pickle
    val up = p.unpickle[Seq[Int]]
    assert(up === Seq(1, 2, 3))
  }

  test("IndexedSeq") {
    val p = IndexedSeq(1, 2, 3).pickle
    val up = p.unpickle[IndexedSeq[Int]]
    assert(up === IndexedSeq(1, 2, 3))
  }

  test("LinearSeq") {
    val p = LinearSeq(1, 2, 3).pickle
    val up = p.unpickle[LinearSeq[Int]]
    assert(up === LinearSeq(1, 2, 3))
  }

  test("Iterable[Person]") {
    val i: Iterable[Person] = Seq(Person(1), Person(2), Person(3))
    val p = i.pickle
    val up = p.unpickle[Iterable[Person]]
    assert(up === i)
  }

  test("Array") {
    val p = Array(Person(1), Person(2), Person(3)).pickle
    val up = p.unpickle[Array[Person]]
    assert(up === Array(Person(1), Person(2), Person(3)))
  }

  test("immutable.Set[String]") {
    val m = Set("a", "b", "c")
    val p = m.pickle
    val up = p.unpickle[Set[String]]
    assert(up === m)
  }

  test("immutable.Set[Person]") {
    val m = Set(Person(1), Person(2), Person(3))
    val p = m.pickle
    val up = p.unpickle[Set[Person]]
    assert(up === m)
  }

  test("immutable.SortedSet") {
    val m = immutable.SortedSet("b", "a", "c")
    val p = m.pickle
    val up = p.unpickle[immutable.SortedSet[String]]
    assert(up === m)
  }

  test("immutable.Map") {
    val m = Map(1 -> "a", 2 -> "b", 3 -> "c")
    val p = m.pickle
    val up = p.unpickle[Map[Int, String]]
    assert(up === m)
  }

  test("immutable.SortedMap") {
    val m = immutable.SortedMap(2 -> "b", 1 -> "a", 3 -> "c")
    val p = m.pickle
    val up = p.unpickle[immutable.SortedMap[Int, String]]
    assert(up === m)
  }

  test("mutable.Set[String]") {
    val m = mutable.Set("a", "b", "c")
    val p = m.pickle
    val up = p.unpickle[mutable.Set[String]]
    assert(up === m)
  }

  test("mutable.Set[Person]") {
    val m = mutable.Set(Person(1), Person(2), Person(3))
    val p = m.pickle
    val up = p.unpickle[mutable.Set[Person]]
    assert(up === m)
  }

  test("mutable.SortedSet") {
    val m = mutable.SortedSet("b", "a", "c")
    val p = m.pickle
    val up = p.unpickle[mutable.SortedSet[String]]
    assert(up === m)
  }

  test("mutable.Map") {
    val m = mutable.Map(1 -> "a", 2 -> "b", 3 -> "c")
    val p = m.pickle
    val up = p.unpickle[mutable.Map[Int, String]]
    assert(up === m)
  }
}
