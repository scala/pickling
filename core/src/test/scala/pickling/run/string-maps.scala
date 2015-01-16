package scala.pickling.test.smaps

import org.scalatest.FunSuite

import scala.pickling._
import AllPicklers._
import json._

import scala.reflect.runtime.universe._
import definitions._

class StringMapsTest extends FunSuite {

  implicit def stringMapPickler[A](implicit valuePickler: SPickler[A], valueUnpickler: Unpickler[A], valueTag: FastTypeTag[A],
    mapTag: FastTypeTag[Map[String, A]],
    keysPickler: SPickler[List[String]], keysUnpickler: Unpickler[List[String]]): SPickler[Map[String, A]] with Unpickler[Map[String, A]] = new SPickler[Map[String, A]] with Unpickler[Map[String, A]] {

    override val tag = mapTag

    def isPrimitive: Boolean = valueTag.tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && eltpe.typeSymbol.isClass && eltpe.typeSymbol.asClass.isPrimitive => true
      case _ => false
    }

    def pickle(m: Map[String, A], builder: PBuilder): Unit = {
      builder.hintTag(mapTag)
      builder.beginEntry(m)
      builder.pushHints()

      builder.putDynamicFieldNames(m.keys.toList.sorted)

      if (isPrimitive) {
        builder.hintStaticallyElidedType()
        builder.hintTag(valueTag)
        builder.pinHints()
      }

      m foreach { kv =>
        builder.putField(kv._1, { b =>
          if (!isPrimitive) b.hintTag(valueTag)
          valuePickler.pickle(kv._2, b)
        })
      }

      builder.popHints()
      builder.endEntry()
    }

    def unpickle(tpe: => FastTypeTag[_], reader: PReader): Any = {
      reader.pushHints()
      val keys = reader.readDynamicFieldNames()

      if (isPrimitive) {
        reader.hintStaticallyElidedType()
        reader.hintTag(valueTag)
        reader.pinHints()
      }

      val results = for (key <- keys) yield {
        val nested = reader.readField(key)
        if (!isPrimitive) nested.hintTag(valueTag)
        nested.beginEntry()
        val value = valueUnpickler.unpickle(valueTag, nested)
        nested.endEntry()
        key -> value.asInstanceOf[A]
      }

      reader.popHints()
      results.toMap
    }
  }

  test("main") {
    val m = Map("a" -> 3, "b" -> 2, "c" -> 1)

    val p = m.pickle
    assert(p.value.toString == """{
      |  "tpe": "scala.collection.immutable.Map[java.lang.String,scala.Int]",
      |  "a": 3,
      |  "b": 2,
      |  "c": 1
      |}""".stripMargin)

    val up = p.unpickle[Map[String, Int]]
    assert(m == up)
  }
}
