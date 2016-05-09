package scala.pickling
package pickler

import scala.collection.generic.CanBuildFrom

// TODO(jvican) - Register runtime pickler generators

trait VectorPicklers {
  implicit def vectorPickler[T : FastTypeTag](
      implicit elemPickler: Pickler[T],
      elemUnpickler: Unpickler[T],
      collTag: FastTypeTag[Vector[T]],
      cbf: CanBuildFrom[Vector[T], T, Vector[T]]
  ): AbstractPicklerUnpickler[Vector[T]] = SeqSetPickler[T, Vector]
}
