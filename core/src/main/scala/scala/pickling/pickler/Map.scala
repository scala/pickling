package scala.pickling
package pickler

import scala.collection.generic.CanBuildFrom
import scala.collection.{ immutable, mutable }
import scala.pickling.internal._

object MapPicklerHelper {
  def tupleTagExtractor[T,U](tpe: FastTypeTag[_]): FastTypeTag[(T, U)] =
    tpe.typeArgs match {
      case List(one, two) => tpe.asInstanceOf[FastTypeTag[(T, U)]]
      // Note: This is what we do to handle empty map types when singleton types are used.
      case List() => FastTypeTag.apply("scala.Tuple2").asInstanceOf[FastTypeTag[(T,U)]]
      case x => throw new PicklingException(s"Error, expected one type argument  on $tpe, found: $x")
    }
}

trait MapPicklers {
  implicit def mapPickler[K: FastTypeTag, V: FastTypeTag](implicit elemPickler: Pickler[(K, V)], elemUnpickler: Unpickler[(K, V)], pairTag: FastTypeTag[(K, V)], collTag: FastTypeTag[Map[K, V]], cbf: CanBuildFrom[Map[K, V], (K, V), Map[K, V]]): Pickler[Map[K, V]] with Unpickler[Map[K, V]] =
    MapPickler[K, V, Map]

  locally {
    val generator =
      TravPickler.generate[(Any, Any), Map[Any, Any]](implicitly[CanBuildFrom[Map[Any, Any], (Any, Any), Map[Any,Any]]], identity[Map[Any, Any]]) { tpe =>
        MapPicklerHelper.tupleTagExtractor(tpe)
      } _
    currentRuntime.picklers.registerPicklerUnpicklerGenerator("scala.collection.immutable.Map", generator)
    currentRuntime.picklers.registerPicklerUnpicklerGenerator("scala.collection.immutable.Map.Map1", generator)
    currentRuntime.picklers.registerPicklerUnpicklerGenerator("scala.collection.immutable.Map.Map2", generator)
    currentRuntime.picklers.registerPicklerUnpicklerGenerator("scala.collection.immutable.Map.Map3", generator)
    currentRuntime.picklers.registerPicklerUnpicklerGenerator("scala.collection.immutable.Map.Map4", generator)
    currentRuntime.picklers.registerPicklerUnpicklerGenerator("scala.collection.immutable.HashMap.HashTrieMap", generator)
  }
}

trait ImmutableSortedMapPicklers {
  implicit def immutableSortedMapPickler[K: FastTypeTag, V: FastTypeTag](implicit elemPickler: Pickler[(K, V)], elemUnpickler: Unpickler[(K, V)], pairTag: FastTypeTag[(K, V)], collTag: FastTypeTag[immutable.SortedMap[K, V]], cbf: CanBuildFrom[immutable.SortedMap[K, V], (K, V), immutable.SortedMap[K, V]]): Pickler[immutable.SortedMap[K, V]] with Unpickler[immutable.SortedMap[K, V]] =
    MapPickler[K, V, immutable.SortedMap]

  // TODO - SortedMap runtime generation involves using a specialized pickler that can remember the ordering of elements.  Currently our pickler does not do that.
}

trait MutableMapPicklers {
  implicit def mutableMapPickler[K: FastTypeTag, V: FastTypeTag](implicit elemPickler: Pickler[(K, V)], elemUnpickler: Unpickler[(K, V)], pairTag: FastTypeTag[(K, V)], collTag: FastTypeTag[mutable.Map[K, V]], cbf: CanBuildFrom[mutable.Map[K, V], (K, V), mutable.Map[K, V]]): Pickler[mutable.Map[K, V]] with Unpickler[mutable.Map[K, V]] =
    MapPickler[K, V, mutable.Map]

  locally {
    val generator =
      TravPickler.generate[(Any, Any), mutable.Map[Any, Any]](implicitly[CanBuildFrom[mutable.Map[Any, Any], (Any, Any), mutable.Map[Any,Any]]], identity[mutable.Map[Any, Any]]) { tpe =>
        MapPicklerHelper.tupleTagExtractor(tpe)
      } _
    currentRuntime.picklers.registerPicklerUnpicklerGenerator("scala.collection.mutable.Map", generator)
  }
}
