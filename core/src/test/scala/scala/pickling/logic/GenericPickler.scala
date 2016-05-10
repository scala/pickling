package scala.pickling.test.generic.pickler

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

case class PersonY(name: String, age: Int)
case class PersonX(name: String, age: Int, salary: Int)
class CustomPersonXPickler(implicit val format: PickleFormat) extends Pickler[PersonX] {
  def tag: FastTypeTag[PersonX] = implicitly[FastTypeTag[PersonX]]

  def pickle(picklee: PersonX, builder: PBuilder) = {
    builder.beginEntry(picklee, tag).putField("name", b => {
      b.beginEntry(picklee.name, FastTypeTag.String)
      b.endEntry()
    })
    builder.endEntry()
  }
}

class GenericPickler extends FunSuite {
  test("stack-overflow-pickle-unpickle") {
    def bar[T: Pickler](t: T) = t.pickle
    def unbar[T: Unpickler](s: String) = JSONPickle(s).unpickle[T]

    val p = PersonY("Philipp", 32)
    assert(bar(p).value == p.pickle.value)
    assert(bar(42).unpickle[Int] == 42)

    val unbarred = unbar[PersonY](bar(p).value)
    assert(unbarred == p)
  }

  test("issue-4") {
    implicit def genCustomPersonXPickler[T <: PersonX](implicit format: PickleFormat) = new CustomPersonXPickler
    def fn[T <: PersonX:  Pickler](x: T) = x.pickle

    val p = PersonX("Philipp", 32, 99999999)
    val jsn = """JSONPickle({
      |  "$type": "scala.pickling.test.generic.pickler.PersonX",
      |  "name": {
      |    "$type": "java.lang.String",
      |    "value": "Philipp"
      |  }
      |})""".stripMargin.trim
    assert(fn(p).toString === jsn)
  }
}