package scala.pickling
package pickler

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.pickling.spi.PicklerRegistry.PicklerUnpicklerGen

trait IterablePicklers extends GeneratorRegistry {

  implicit def iterablePickler[T : FastTypeTag](
      implicit elemPickler: Pickler[T],
      elemUnpickler: Unpickler[T],
      collTag: FastTypeTag[Iterable[T]],
      cbf: CanBuildFrom[Iterable[T], T, Iterable[T]]
  ): AbstractPicklerUnpickler[Iterable[T]] = TravPickler[T, Iterable[T]]

  implicit def listPickler[T : FastTypeTag](
      implicit elemPickler: Pickler[T],
      elemUnpickler: Unpickler[T],
      colTag: FastTypeTag[List[T]],
      cbf: CanBuildFrom[List[T], T, List[T]]
  ): AbstractPicklerUnpickler[List[T]] = TravPickler[T, List[T]]

  locally {

    val cbf = implicitly[CanBuildFrom[List[Any], Any, List[Any]]]
    val generator: PicklerUnpicklerGen[List[Any]] = {
      TravPickler.generate(cbf, identity[List[Any]]) { tpe =>
        TravPickler.oneArgumentTagExtractor(tpe)
      }
    }

    // TODO: Deserialize directly Nil and remove this hack.
    registerGen("scala.collection.immutable.Nil.type", generator)
    registerGen("scala.collection.immutable.$colon$colon", generator)
    registerGen("scala.collection.immutable.List", generator)
  }

  locally {

    val cbf = implicitly[CanBuildFrom[Iterable[Any], Any, Iterable[Any]]]
    val generator: PicklerUnpicklerGen[Iterable[Any]] = {
      TravPickler.generate(cbf, identity[Iterable[Any]]) { tpe =>
        TravPickler.oneArgumentTagExtractor(tpe)
      }
    }

    registerGen("scala.collection.Iterable", generator)
  }
}

object TravPickler extends GeneratorHelper {

  /** Creates a pickling generator for any [[Traversable]] to be used at runtime. */
  def generate[T, C](cbf: CanBuildFrom[C, T, C], asTrav: C => Traversable[_])(
      elementTagExtractor: FastTypeTagSpecializer[T])(
      tpe: FastTypeTag[_]): AbstractPicklerUnpickler[C] = {

    val (elemPickler, elemUnpickler) = generateHelper(elementTagExtractor)(tpe)
    apply[T, C](asTrav, elemPickler, elemUnpickler, cbf, specialize[C](tpe))
  }

  def apply[T, C](implicit asTrav: C => Traversable[_],
                  elemPickler: Pickler[T],
                  elemUnpickler: Unpickler[T],
                  cbf: CanBuildFrom[C, T, C],
                  collTag: FastTypeTag[C]): AbstractPicklerUnpickler[C] = {
    new AbstractPicklerUnpickler[C] with AutoRegister[C] {

      val elemTag = elemPickler.tag
      val isPrimitive = elemTag.isEffectivelyPrimitive

      def tag: FastTypeTag[C] = collTag

      def pickle(coll: C, builder: PBuilder): Unit = {
        // TODO Can we do the same for other type tags?
        if (elemTag == FastTypeTag.Int)
          builder.hintKnownSize(coll.size * 4 + 100)
        builder.beginEntry(coll, tag)
        builder.beginCollection(coll.size)

        builder.pushHints()
        if (isPrimitive) {
          builder.hintElidedType(elemTag)
          builder.pinHints()
        }

        (coll: Traversable[_]).asInstanceOf[Traversable[T]].foreach {
          (elem: T) =>
            builder putElement { b =>
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
          reader.hintElidedType(elemTag)
          reader.pinHints()
        }

        val length = reader.readLength()
        val builder = cbf.apply() // builder with element type T
        var i = 0
        while (i < length) {
          val r = reader.readElement()
          val elem = elemUnpickler.unpickleEntry(r)
          builder += elem.asInstanceOf[T]
          i = i + 1
        }

        preader.popHints()
        preader.endCollection()
        builder.result
      }
    }
  }
}

object SeqSetPickler {
  def apply[T : FastTypeTag, Coll[_] <: Traversable[_]](
      implicit elemPickler: Pickler[T],
      elemUnpickler: Unpickler[T],
      cbf: CanBuildFrom[Coll[T], T, Coll[T]],
      collTag: FastTypeTag[Coll[T]]
  ): AbstractPicklerUnpickler[Coll[T]] = TravPickler[T, Coll[T]]
}

object MapPickler {
  def apply[K : FastTypeTag, V : FastTypeTag, M[_, _] <: collection.Map[_, _]](
      implicit elemPickler: Pickler[(K, V)],
      elemUnpickler: Unpickler[(K, V)],
      cbf: CanBuildFrom[M[K, V], (K, V), M[K, V]],
      collTag: FastTypeTag[M[K, V]]
  ): AbstractPicklerUnpickler[M[K, V]] = TravPickler[(K, V), M[K, V]]
}
