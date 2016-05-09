package scala.pickling
package pickler

import scala.collection.generic.CanBuildFrom
import scala.collection.{immutable, mutable}
import scala.pickling.PicklingErrors.TypeMismatch
import scala.pickling.spi.PicklerRegistry.PicklerUnpicklerGen

object MapPicklerHelper {
  def tupleTagExtractor[T, U](tpe: FastTypeTag[_]): FastTypeTag[(T, U)] =
    tpe.typeArgs match {
      case List(one, two) => tpe.asInstanceOf[FastTypeTag[(T, U)]]
      // Note: This is what we do to handle empty map types when singleton types are used.
      case List() =>
        FastTypeTag.apply("scala.Tuple2").asInstanceOf[FastTypeTag[(T, U)]]
      case x => throw TypeMismatch(List(tpe), x)
    }
}

trait MapPicklers extends GeneratorRegistry {

  implicit def mapPickler[K : FastTypeTag, V : FastTypeTag](
      implicit elemPickler: Pickler[(K, V)],
      elemUnpickler: Unpickler[(K, V)],
      pairTag: FastTypeTag[(K, V)],
      collTag: FastTypeTag[Map[K, V]],
      cbf: CanBuildFrom[Map[K, V], (K, V), Map[K, V]]
  ): AbstractPicklerUnpickler[Map[K, V]] = MapPickler[K, V, Map]

  locally {

    val cbf =
      implicitly[CanBuildFrom[Map[Any, Any], (Any, Any), Map[Any, Any]]]
    val generator: PicklerUnpicklerGen[Map[Any, Any]] =
      TravPickler.generate(cbf, identity[Map[Any, Any]]) { tpe =>
        MapPicklerHelper.tupleTagExtractor(tpe)
      }

    registerGen("scala.collection.immutable.Map", generator)
    registerGen("scala.collection.immutable.Map.Map1", generator)
    registerGen("scala.collection.immutable.Map.Map2", generator)
    registerGen("scala.collection.immutable.Map.Map3", generator)
    registerGen("scala.collection.immutable.Map.Map4", generator)
    registerGen("scala.collection.immutable.HashMap.HashTrieMap", generator)
  }
}

trait ImmutableSortedMapPicklers {

  implicit def immutableSortedMapPickler[K : FastTypeTag, V : FastTypeTag](
      implicit elemPickler: Pickler[(K, V)],
      elemUnpickler: Unpickler[(K, V)],
      pairTag: FastTypeTag[(K, V)],
      collTag: FastTypeTag[immutable.SortedMap[K, V]],
      cbf: CanBuildFrom[
          immutable.SortedMap[K, V], (K, V), immutable.SortedMap[K, V]]
  ): AbstractPicklerUnpickler[immutable.SortedMap[K, V]] =
    MapPickler[K, V, immutable.SortedMap]

  // TODO - SortedMap runtime generation involves using a specialized pickler that can remember the ordering of elements.  Currently our pickler does not do that.
}

trait MutableMapPicklers extends GeneratorRegistry {

  import mutable.{Map => Mmap}

  implicit def mutableMapPickler[K : FastTypeTag, V : FastTypeTag](
      implicit elemPickler: Pickler[(K, V)],
      elemUnpickler: Unpickler[(K, V)],
      pairTag: FastTypeTag[(K, V)],
      collTag: FastTypeTag[Mmap[K, V]],
      cbf: CanBuildFrom[Mmap[K, V], (K, V), Mmap[K, V]]
  ): AbstractPicklerUnpickler[Mmap[K, V]] = MapPickler[K, V, Mmap]

  locally {

    val cbf =
      implicitly[CanBuildFrom[Mmap[Any, Any], (Any, Any), Mmap[Any, Any]]]
    val generator: PicklerUnpicklerGen[Mmap[Any, Any]] =
      TravPickler.generate(cbf, identity[Mmap[Any, Any]]) { tpe =>
        MapPicklerHelper.tupleTagExtractor(tpe)
      }

    registerGen("scala.collection.mutable.Map", generator)
  }
}
