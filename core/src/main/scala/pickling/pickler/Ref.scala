package scala.pickling
package pickler

trait RefPicklers {
  implicit def refPickler: Pickler[refs.Ref] = throw new Error("cannot pickle refs") // TODO: make this a macro
  implicit val refUnpickler: Unpickler[refs.Ref] = PrimitivePickler[refs.Ref]
}
