package scala.pickling
package pickler

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer

trait ArrayBufferPicklers {
  // TODO(jsuereth) - Add pickler generator
  implicit def arrayBufferPickler[T: FastTypeTag](implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
    collTag: FastTypeTag[ArrayBuffer[T]], cbf: CanBuildFrom[ArrayBuffer[T], T, ArrayBuffer[T]]):
    Pickler[ArrayBuffer[T]] with Unpickler[ArrayBuffer[T]] =
    SeqSetPickler[T, ArrayBuffer]
}
