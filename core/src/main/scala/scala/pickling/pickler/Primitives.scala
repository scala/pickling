package scala.pickling
package pickler

import scala.pickling.PicklingErrors.{FailedUnpickling, BasePicklingException}

/** Generate [[Pickler]]s and [[Unpickler]]s for the primitive types.
  *
  * The primitive types are [[Byte]], [[Short]], [[Char]], [[Int]], [[Long]],
  * [[Boolean]], [[Float]], [[Double]], [[Null]], [[String]] and [[Unit]].
  */
trait PrimitivePicklers {
  implicit val bytePickler: AbstractPicklerUnpickler[Byte] = PrimitivePickler[Byte]
  implicit val shortPickler: AbstractPicklerUnpickler[Short] = PrimitivePickler[Short]
  implicit val charPickler: AbstractPicklerUnpickler[Char] = PrimitivePickler[Char]
  implicit val intPickler: AbstractPicklerUnpickler[Int] = PrimitivePickler[Int]
  implicit val longPickler: AbstractPicklerUnpickler[Long] = PrimitivePickler[Long]
  implicit val booleanPickler: AbstractPicklerUnpickler[Boolean] = PrimitivePickler[Boolean]
  implicit val floatPickler: AbstractPicklerUnpickler[Float] = PrimitivePickler[Float]
  implicit val doublePickler: AbstractPicklerUnpickler[Double] = PrimitivePickler[Double]
  implicit val nullPickler: AbstractPicklerUnpickler[Null] = PrimitivePickler[Null]
  implicit val stringPickler: AbstractPicklerUnpickler[String] = PrimitivePickler[String]
  implicit val unitPickler: AbstractPicklerUnpickler[Unit] = PrimitivePickler[Unit]
}

class PrimitivePickler[T: FastTypeTag](name: String)
  extends AbstractPicklerUnpickler[T] with AutoRegister[T] {

  override def tag = implicitly[FastTypeTag[T]]
  override def pickle(picklee: T, builder: PBuilder): Unit = {
    builder.beginEntry(picklee, tag)
    builder.endEntry()
  }
  override def unpickle(tag: String, reader: PReader): Any = {
    try {
      // Don't use beginEntry/endEntry because
      // tag has to be elided and there's some
      // incompatibilities between java and scala
      // primitive types (like String or Int)
      reader.readPrimitive()
    } catch { case BasePicklingException(msg, cause) =>
        throw FailedUnpickling(name, tag, msg, cause)
    }
  }
}
object PrimitivePickler {
  def apply[A: FastTypeTag]: AbstractPicklerUnpickler[A] =
    new PrimitivePickler[A](FastTypeTag.valueTypeName(implicitly[FastTypeTag[A]]))
}
