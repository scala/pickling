package scala.pickling
package runtime

import scala.reflect.{runtime => reflectRuntime}
import internal._

// TODO - Move all these into the "PicklerRegistry"
object CustomRuntime {

  def mkRuntimeTravPickler[C <% Traversable[_]](elemClass: Class[_], elemTag: FastTypeTag[_], collTag: FastTypeTag[_],
                                                elemPickler0: Pickler[_], elemUnpickler0: Unpickler[_]):
    Pickler[C] with Unpickler[C] = new Pickler[C] with Unpickler[C] {

    val elemPickler   = elemPickler0.asInstanceOf[Pickler[AnyRef]]
    val elemUnpickler = elemUnpickler0.asInstanceOf[Unpickler[AnyRef]]

    val isPrimitive = elemTag.isEffectivelyPrimitive

    def tag: FastTypeTag[C] = collTag.asInstanceOf[FastTypeTag[C]]

    def pickle(coll: C, builder: PBuilder): Unit = {
      builder.beginEntry(coll, tag)
      builder.beginCollection(coll.size)

      builder.pushHints()
      if (isPrimitive) {
        builder.hintElidedType(elemTag)
        builder.pinHints()
      }

      (coll: Traversable[_]).asInstanceOf[Traversable[AnyRef]].foreach { (elem: AnyRef) =>
        builder putElement { b =>
          elemPickler.pickle(elem, b)
        }
      }

      builder.popHints()
      builder.endCollection()
      builder.endEntry()
    }

    def unpickle(tag: String, preader: PReader): Any = {
      val reader = preader.beginCollection()

      preader.pushHints()
      if (isPrimitive) {
        reader.hintElidedType(elemTag)
        reader.pinHints()
      }

      val length = reader.readLength()
      val newArray = java.lang.reflect.Array.newInstance(elemClass, length).asInstanceOf[Array[AnyRef]]

      var i = 0
      while (i < length) {
        try {
          val r = reader.readElement()
          val elem = elemUnpickler.unpickleEntry(r)
          newArray(i) = elem.asInstanceOf[AnyRef]
          i = i + 1
        } catch {
          case PicklingException(msg, cause) =>
            throw PicklingException(s"""error in unpickle of 'mkRuntimeTravPickler':
                                       |collTag: '${collTag.key}'
                                       |elemTag: '${elemTag.key}'
                                       |message:
                                       |$msg""".stripMargin, cause)
          case e: Exception =>
            e.printStackTrace()
            throw PicklingException(s"""exception in unpickle of 'mkRuntimeTravPickler':
                                       |collTag: '${collTag.key}'
                                       |elemTag: '${elemTag.key}'""".stripMargin, Some(e))
        }
      }

      preader.popHints()
      preader.endCollection()
      newArray
    }
  }

}

class Tuple2RTKnownTagUnpickler[L, R](lhs: Unpickler[L], rhs: Unpickler[R]) extends AbstractUnpickler[(L,R)] {
  def unpickleField[T](name: String, reader: PReader, unpickler: Unpickler[T]): T = {
    val reader1 = reader.readField(name)
    // TODO - Always elide tags?
    if(unpickler.tag.isEffectivelyPrimitive) reader1.hintElidedType(unpickler.tag)
    unpickler.unpickleEntry(reader1).asInstanceOf[T]
  }
  override def unpickle(tag: String, reader: PReader): Any = {
    (unpickleField("_1", reader, lhs), unpickleField("_2", reader, rhs))
  }
  override def tag: FastTypeTag[(L, R)] =
    FastTypeTag.apply(s"scala.Tuple2[${lhs.tag.key},${rhs.tag.key}}]").asInstanceOf[FastTypeTag[(L,R)]]
}

// TODO - This pickler should actually use the known tag if it is passed.  Currently it is never used.
class Tuple2RTPickler() extends AbstractPicklerUnpickler[(Any, Any)] {
  val tag = FastTypeTag[(Any, Any)]("scala.Tuple2[scala.Any, scala.Any]")

  def pickleField(name: String, value: Any, builder: PBuilder): Unit = {
    val (tag1, pickler1) = if (value == null) {
      (FastTypeTag.Null.asInstanceOf[FastTypeTag[Any]], Defaults.nullPickler.asInstanceOf[Pickler[Any]])
    } else {
      val clazz = value.getClass
      val tag = FastTypeTag.makeRaw(clazz).asInstanceOf[FastTypeTag[Any]]
      val pickler = scala.pickling.internal.currentRuntime.picklers.genPickler(clazz.getClassLoader, clazz, tag).asInstanceOf[Pickler[Any]]
      (tag, pickler)
    }

    builder.putField(name, b => {
      pickler1.pickle(value, b)
    })
  }

  def pickle(picklee: (Any, Any), builder: PBuilder): Unit = {
    // println(s"@@@ using runtime ${this.getClass.getName}")
    builder.beginEntry(picklee, tag)

    val fld1 = picklee._1
    pickleField("_1", fld1, builder)

    val fld2 = picklee._2
    pickleField("_2", fld2, builder)

    builder.endEntry()

    // val specialPickler = new SpecialTuple2Pickler(tag1, pickler1, tag2, pickler2)
    // SpecialTuple2Pickler.classSelection += ((class1 -> class2) -> Selection(specialPickler, tag))
    // println(s"@@@ registered dynamic specialized pickler ${specialPickler.getClass.getName}")
  }

  def unpickleField(name: String, reader: PReader): Any = {
    val reader1 = reader.readField(name)
    val tag1 = reader1.beginEntry()

    val value = {
      if (reader1.atPrimitive) {
        reader1.readPrimitive()
      } else {
        val unpickler1 = internal.currentRuntime.picklers.genUnpickler(reflectRuntime.currentMirror, tag1)
        try {
          unpickler1.unpickle(tag1, reader1)
        } catch {
          case PicklingException(msg, cause) =>
            throw PicklingException(s"""error in unpickle of '${this.getClass.getName}':
                                       |field name: '$name'
                                       |field tag: '${tag1}'
                                       |message:
                                       |$msg""".stripMargin, cause)
        }
      }
    }
    reader1.endEntry()
    value
  }

  def unpickle(tag: String, reader: PReader): Any = {
    val fld1 = unpickleField("_1", reader)
    val fld2 = unpickleField("_2", reader)
    (fld1, fld2)
  }
}
