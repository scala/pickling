package scala.pickling
package pickler

import scala.collection.generic.CanBuildFrom

trait ArrayPicklers {
  implicit def arrayPickler[T >: Null: FastTypeTag](implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
    collTag: FastTypeTag[Array[T]], cbf: CanBuildFrom[Array[T], T, Array[T]]):
    Pickler[Array[T]] with Unpickler[Array[T]] = TravPickler[T, Array[T]]
}
