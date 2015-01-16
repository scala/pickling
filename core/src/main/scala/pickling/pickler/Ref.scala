package scala.pickling.pickler

import scala.pickling._

trait RefPicklers {
  implicit def refPickler: SPickler[refs.Ref] = throw new Error("cannot pickle refs") // TODO: make this a macro
  implicit val refUnpickler: Unpickler[refs.Ref] = PrimitivePickler[refs.Ref]
}

object RefPicklers extends RefPicklers {}
