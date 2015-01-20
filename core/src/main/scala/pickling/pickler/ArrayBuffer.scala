package scala.pickling.pickler

import scala.pickling._
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer

trait ArrayBufferPicklers {
  implicit def arrayBufferPickler[T: FastTypeTag](implicit elemPickler: SPickler[T], elemUnpickler: Unpickler[T],
    collTag: FastTypeTag[ArrayBuffer[T]], cbf: CanBuildFrom[ArrayBuffer[T], T, ArrayBuffer[T]]):
    SPickler[ArrayBuffer[T]] with Unpickler[ArrayBuffer[T]] =
    SeqSetPickler[T, ArrayBuffer]
}
