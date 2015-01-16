package scala.pickling.pickler

import scala.pickling._
import scala.collection.generic.CanBuildFrom
import scala.collection.{ IndexedSeq, LinearSeq }

trait SeqPicklers {
  implicit def seqPickler[T: FastTypeTag](implicit elemPickler: SPickler[T], elemUnpickler: Unpickler[T],
    collTag: FastTypeTag[Seq[T]], cbf: CanBuildFrom[Seq[T], T, Seq[T]]): SPickler[Seq[T]] with Unpickler[Seq[T]] =
    SeqSetPickler[T, Seq]
}

object SeqPicklers extends SeqPicklers {}

trait IndexedSeqPicklers {
  implicit def indexedSeqPickler[T: FastTypeTag](implicit elemPickler: SPickler[T], elemUnpickler: Unpickler[T],
    collTag: FastTypeTag[IndexedSeq[T]], cbf: CanBuildFrom[IndexedSeq[T], T, IndexedSeq[T]]):
    SPickler[IndexedSeq[T]] with Unpickler[IndexedSeq[T]] =
    SeqSetPickler[T, IndexedSeq]
}

object IndexedSeqPicklers extends IndexedSeqPicklers {}

trait LinearSeqPicklers {
  implicit def linearSeqPickler[T: FastTypeTag](implicit elemPickler: SPickler[T], elemUnpickler: Unpickler[T],
    collTag: FastTypeTag[LinearSeq[T]], cbf: CanBuildFrom[LinearSeq[T], T, LinearSeq[T]]):
    SPickler[LinearSeq[T]] with Unpickler[LinearSeq[T]] =
    SeqSetPickler[T, LinearSeq]
}

object LinearSeqPicklers extends LinearSeqPicklers {}
