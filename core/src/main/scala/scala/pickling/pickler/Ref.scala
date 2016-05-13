package scala.pickling
package pickler

import scala.pickling.PicklingErrors.LogicPicklingError

@deprecated("Sharing is not guaranteed to be safe w/ all possible picklers.")
trait RefPicklers {

  @deprecated("Sharing is not guaranteed to be safe w/ all possible picklers.")
  implicit val refPicklerUnpickler: AbstractPicklerUnpickler[refs.Ref] = {
    val name = FastTypeTag.valueTypeName(implicitly[FastTypeTag[refs.Ref]])
    new PrimitivePickler[refs.Ref](name) {
      final override def pickle(picklee: refs.Ref, builder: PBuilder): Unit = {
        throw LogicPicklingError("`Ref`s cannot be pickled, only unpickled.")
      }
    }
  }

  @deprecated("Sharing is not guaranteed to be safe w/ all possible picklers.")
  val refPickler: Pickler[refs.Ref] = refPicklerUnpickler

  @deprecated("Use `refPicklerUnpickler` instead, this one is not implicit anymore", "0.11.0")
  val refUnpickler: Unpickler[refs.Ref] = refPicklerUnpickler

}
