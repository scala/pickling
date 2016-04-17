package scala.pickling
package pickler

import scala.pickling.internal._
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

trait IterablePicklers {
  implicit def iterablePickler[T: FastTypeTag](implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
    collTag: FastTypeTag[Iterable[T]], cbf: CanBuildFrom[Iterable[T], T, Iterable[T]]):
    Pickler[Iterable[T]] with Unpickler[Iterable[T]] = TravPickler[T, Iterable[T]]


  implicit def listPickler[T: FastTypeTag](implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T], colTag: FastTypeTag[List[T]], cbf: CanBuildFrom[List[T], T, List[T]]):
     Pickler[List[T]] with Unpickler[List[T]] = TravPickler[T, List[T]]

  // Register List runtime pickler
  locally {
    val generator =
      TravPickler.generate(implicitly[CanBuildFrom[List[Any], Any, List[Any]]], identity[List[Any]]) { tpe =>
        TravPickler.oneArgumentTagExtractor(tpe)
      } _
    currentRuntime.picklers.registerPicklerUnpicklerGenerator("scala.collection.immutable.List", generator)
    currentRuntime.picklers.registerPicklerUnpicklerGenerator("scala.collection.immutable.$colon$colon", generator)
    // Note: this is kind of wrong/backward.  We should just directly deserialize Nil
    currentRuntime.picklers.registerPicklerUnpicklerGenerator("scala.collection.immutable.Nil.type", generator)
  }

  // Register Iterable runtime pickler
  locally {
    val generator =
      TravPickler.generate(implicitly[CanBuildFrom[Iterable[Any], Any, Iterable[Any]]], identity[Iterable[Any]]) { tpe =>
        TravPickler.oneArgumentTagExtractor(tpe)
      } _
    currentRuntime.picklers.registerPicklerUnpicklerGenerator("scala.collection.Iterable", generator)
  }
}

object TravPickler {
  private val ANY_TAG = FastTypeTag.Any

  def oneArgumentTagExtractor[T](tpe: FastTypeTag[_]): FastTypeTag[T] = {
    tpe.typeArgs match {
      case List(one) => one.asInstanceOf[FastTypeTag[T]]
        // Note: This is hack to handle "Nil.type" as a tag, which really shouldn't happen.
        // We should be able to remove this code.
      case List() => ANY_TAG.asInstanceOf[FastTypeTag[T]]
      case x => throw new PicklingException(s"Error, expected one type argument  on $tpe, found: $x")
    }
  }

  /** Creates a pickling generator that can be registered at runtime. */
  def generate[T, C](cbf: CanBuildFrom[C,T,C], asTraversable: C => Traversable[_])(elementTagExtractor: FastTypeTag[_] => FastTypeTag[T])(tpe: FastTypeTag[_]): AbstractPicklerUnpickler[C] = {
    val elementType = elementTagExtractor(tpe)
    val elemPickler =
      if(elementType.key == ANY_TAG.key) AnyPicklerUnpickler
      else currentRuntime.picklers.lookupPickler(elementType.key).getOrElse(
        throw new PicklingException(s"Cannnot generate a pickler/unpickler for $tpe, cannot find a pickler for $elementType"))
    val elemUnpickler =
      if(elementType.key == ANY_TAG.key) AnyPicklerUnpickler
      else currentRuntime.picklers.lookupUnpickler(elementType.key).getOrElse(
        throw new PicklingException(s"Cannnot generate a pickler/unpickler for $tpe, cannot find an unpickler for $elementType"))
    val colTag = tpe
    apply[T,C](asTraversable, elemPickler.asInstanceOf[Pickler[T]], elemUnpickler.asInstanceOf[Unpickler[T]], cbf, colTag.asInstanceOf[FastTypeTag[C]])
  }


  def apply[T, C <% Traversable[_]]
    (implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
              cbf: CanBuildFrom[C, T, C], collTag: FastTypeTag[C]): AbstractPicklerUnpickler[C] =
    new AbstractPicklerUnpickler[C] with AutoRegister[C] {

    val elemTag  = elemPickler.tag
    val isPrimitive = elemTag.isEffectivelyPrimitive

    def tag: FastTypeTag[C] = collTag

    def pickle(coll: C, builder: PBuilder): Unit = {
      if (elemTag == FastTypeTag.Int) builder.hintKnownSize(coll.size * 4 + 100)
      builder.beginEntry(coll, tag)
      builder.beginCollection(coll.size)

      builder.pushHints()
      if (isPrimitive) {
        builder.hintElidedType(elemTag)
        builder.pinHints()
      }

      (coll: Traversable[_]).asInstanceOf[Traversable[T]].foreach { (elem: T) =>
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

object SeqSetPickler {
  def apply[T: FastTypeTag, Coll[_] <: Traversable[_]]
    (implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
              cbf: CanBuildFrom[Coll[T], T, Coll[T]],
              collTag: FastTypeTag[Coll[T]]): Pickler[Coll[T]] with Unpickler[Coll[T]] =
    TravPickler[T, Coll[T]]
}

object MapPickler {
  def apply[K: FastTypeTag, V: FastTypeTag, M[_, _] <: collection.Map[_, _]]
    (implicit elemPickler: Pickler[(K, V)], elemUnpickler: Unpickler[(K, V)],
              cbf: CanBuildFrom[M[K, V], (K, V), M[K, V]],
              collTag: FastTypeTag[M[K, V]]): Pickler[M[K, V]] with Unpickler[M[K, V]] =
    TravPickler[(K, V), M[K, V]]
}
