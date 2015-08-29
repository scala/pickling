package scala.pickling
package pickler

import scala.collection.generic.CanBuildFrom
import scala.collection.{ immutable, mutable }

// TODO(jsuereth) - Register runtime pickler generators

trait SetPicklers {
  implicit def setPickler[T: FastTypeTag](implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
    collTag: FastTypeTag[Set[T]], cbf: CanBuildFrom[Set[T], T, Set[T]]): Pickler[Set[T]] with Unpickler[Set[T]] =
    SeqSetPickler[T, Set]
}

trait ImmutableSortedSetPicklers {
  implicit def immutableSortedSetPickler[T: FastTypeTag](implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
    collTag: FastTypeTag[immutable.SortedSet[T]], cbf: CanBuildFrom[immutable.SortedSet[T], T, immutable.SortedSet[T]]):
    Pickler[immutable.SortedSet[T]] with Unpickler[immutable.SortedSet[T]] =
    SeqSetPickler[T, immutable.SortedSet]
}

trait MutableSetPicklers {
  implicit def mutableSetPickler[T: FastTypeTag](implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
    collTag: FastTypeTag[mutable.Set[T]], cbf: CanBuildFrom[mutable.Set[T], T, mutable.Set[T]]):
    Pickler[mutable.Set[T]] with Unpickler[mutable.Set[T]] =
    SeqSetPickler[T, mutable.Set]
}

trait MutableSortedSetPicklers {
  implicit def mutableSortedSetPickler[T: FastTypeTag](implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
    collTag: FastTypeTag[mutable.SortedSet[T]], cbf: CanBuildFrom[mutable.SortedSet[T], T, mutable.SortedSet[T]]):
    Pickler[mutable.SortedSet[T]] with Unpickler[mutable.SortedSet[T]] =
    SeqSetPickler[T, mutable.SortedSet]
}
