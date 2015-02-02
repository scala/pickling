package scala.pickling
package pickler

import scala.pickling.internal._
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

trait IterablePicklers {
  implicit def iterablePickler[T: FastTypeTag](implicit elemPickler: SPickler[T], elemUnpickler: Unpickler[T],
    collTag: FastTypeTag[Iterable[T]], cbf: CanBuildFrom[Iterable[T], T, Iterable[T]]):
    SPickler[Iterable[T]] with Unpickler[Iterable[T]] = TravPickler[T, Iterable[T]]
}

object TravPickler {
  def apply[T: FastTypeTag, C <% Traversable[_]]
    (implicit elemPickler: SPickler[T], elemUnpickler: Unpickler[T],
              cbf: CanBuildFrom[C, T, C], collTag: FastTypeTag[C]): SPickler[C] with Unpickler[C] =
    new SPickler[C] with Unpickler[C] {

    val elemTag  = implicitly[FastTypeTag[T]]
    val isPrimitive = elemTag.isEffectivelyPrimitive

    def tag: FastTypeTag[C] = collTag

    def pickle(coll: C, builder: PBuilder): Unit = {
      if (elemTag == FastTypeTag.Int) builder.hintKnownSize(coll.size * 4 + 100)
      builder.beginEntry(coll)
      builder.beginCollection(coll.size)

      builder.pushHints()
      if (isPrimitive) {
        builder.hintStaticallyElidedType()
        builder.hintTag(elemTag)
        builder.pinHints()
      }

      (coll: Traversable[_]).asInstanceOf[Traversable[T]].foreach { (elem: T) =>
        builder putElement { b =>
          if (!isPrimitive) b.hintTag(elemTag)
          elemPickler.pickle(elem, b)
        }
      }

      builder.popHints()
      builder.endCollection()
      builder.endEntry()
    }

    def unpickle(tpe: String, preader: PReader): Any = {
      val reader = preader.beginCollection()

      preader.pushHints()
      if (isPrimitive) {
        reader.hintStaticallyElidedType()
        reader.hintTag(elemTag)
        reader.pinHints()
      }

      val length = reader.readLength()
      val builder = cbf.apply() // builder with element type T
      var i = 0
      while (i < length) {
        val r = reader.readElement()
        r.beginEntry()
        val elem = elemUnpickler.unpickle(elemTag.key, r)
        r.endEntry()
        builder += elem.asInstanceOf[T]
        i = i + 1
      }

      preader.popHints()
      preader.endCollection()
      builder.result
    }
  }
}

object SeqSetPickler {
  def apply[T: FastTypeTag, Coll[_] <: Traversable[_]]
    (implicit elemPickler: SPickler[T], elemUnpickler: Unpickler[T],
              cbf: CanBuildFrom[Coll[T], T, Coll[T]],
              collTag: FastTypeTag[Coll[T]]): SPickler[Coll[T]] with Unpickler[Coll[T]] =
    TravPickler[T, Coll[T]]
}

object MapPickler {
  def apply[K: FastTypeTag, V: FastTypeTag, M[_, _] <: collection.Map[_, _]]
    (implicit elemPickler: SPickler[(K, V)], elemUnpickler: Unpickler[(K, V)],
              cbf: CanBuildFrom[M[K, V], (K, V), M[K, V]],
              pairTag: FastTypeTag[(K, V)], collTag: FastTypeTag[M[K, V]]): SPickler[M[K, V]] with Unpickler[M[K, V]] =
    TravPickler[(K, V), M[K, V]]
}
