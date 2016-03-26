package scala.pickling
package pickler

@deprecated("Sharing is not guaranteed to be safe w/ all possible picklers.")
trait RefPicklers {
  @deprecated("Sharing is not guaranteed to be safe w/ all possible picklers.")
  implicit def refPickler: Pickler[refs.Ref] = throw new Error("cannot pickle refs")
  implicit val refUnpickler: Unpickler[refs.Ref] = PrimitivePickler[refs.Ref]
}
