package scala.pickling
package pickler

// TODO - We need to rethink structural sharing and references, fundamentally.
trait RefPicklers {
  implicit def refPickler: Pickler[refs.Ref] = throw new Error("cannot pickle refs")
  implicit val refUnpickler: Unpickler[refs.Ref] = PrimitivePickler[refs.Ref]
}
