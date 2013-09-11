package scala.pickling.test.generic.spickler

import org.scalatest.FunSuite
import scala.pickling._
import json._

case class PersonY(name: String, age: Int)
case class PersonX(name: String, age: Int, salary: Int)
class CustomPersonXPickler(implicit val format: PickleFormat) extends SPickler[PersonX] {
  def pickle(picklee: PersonX, builder: PBuilder) = {
    builder.hintTag(implicitly[FastTypeTag[PersonX]])
    builder.beginEntry(picklee).putField("name", b => {
      b.hintTag(FastTypeTag.ScalaString)
      b.beginEntry(picklee.name)
      b.endEntry()
    })
    builder.endEntry()
  }
}

class GenericSpickler extends FunSuite {
  test("stack-overflow-pickle-unpickle") {
    def bar[T: SPickler: FastTypeTag](t: T) = t.pickle
    def unbar[T: Unpickler: FastTypeTag](s: String) = JSONPickle(s).unpickle[T]

    val p = PersonY("Philipp", 32)
    assert(bar(p).value == p.pickle.value)
    assert(bar(42).unpickle[Int] == 42)

    val unbarred = unbar[PersonY](bar(p).value)
    assert(unbarred == p)
  }

  test("issue-4") {
    implicit def genCustomPersonXPickler[T <: PersonX](implicit format: PickleFormat) = new CustomPersonXPickler
    def fn[T <: PersonX:  SPickler: FastTypeTag](x: T) = x.pickle

    val p = PersonX("Philipp", 32, 99999999)
    val jsn = """JSONPickle({
      |  "tpe": "scala.pickling.test.generic.spickler.PersonX",
      |  "name": {
      |    "tpe": "java.lang.String",
      |    "value": "Philipp"
      |  }
      |})""".stripMargin.trim
    assert(fn(p).toString === jsn)
  }
}