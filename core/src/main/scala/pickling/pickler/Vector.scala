package scala.pickling
package pickler

import scala.collection.generic.CanBuildFrom

trait VectorPicklers {
  implicit def vectorPickler[T: FastTypeTag](implicit elemPickler: SPickler[T],
    elemUnpickler: Unpickler[T], collTag: FastTypeTag[Vector[T]], cbf: CanBuildFrom[Vector[T], T, Vector[T]]):
    SPickler[Vector[T]] with Unpickler[Vector[T]] =
    SeqSetPickler[T, Vector]
}

object VectorPicklers extends VectorPicklers {}
