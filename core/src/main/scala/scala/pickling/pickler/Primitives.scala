package scala.pickling
package pickler

import scala.pickling.PicklingErrors.{FailedUnpickling, BasePicklingException}

/** Generate [[Pickler]]s and [[Unpickler]]s for the primitive types.
  *
  * The primitive types are [[Byte]], [[Short]], [[Char]], [[Int]], [[Long]],
  * [[Boolean]], [[Float]], [[Double]], [[Null]], [[String]] and [[Unit]].
  */
trait PrimitivePicklers {
  implicit val bytePickler: Pickler[Byte] with Unpickler[Byte] = PrimitivePickler[Byte]
  implicit val shortPickler: Pickler[Short] with Unpickler[Short] = PrimitivePickler[Short]
  implicit val charPickler: Pickler[Char] with Unpickler[Char] = PrimitivePickler[Char]
  implicit val intPickler: Pickler[Int] with Unpickler[Int] = PrimitivePickler[Int]
  implicit val longPickler: Pickler[Long] with Unpickler[Long] = PrimitivePickler[Long]
  implicit val booleanPickler: Pickler[Boolean] with Unpickler[Boolean] = PrimitivePickler[Boolean]
  implicit val floatPickler: Pickler[Float] with Unpickler[Float] = PrimitivePickler[Float]
  implicit val doublePickler: Pickler[Double] with Unpickler[Double] = PrimitivePickler[Double]
  implicit val nullPickler: Pickler[Null] with Unpickler[Null] = PrimitivePickler[Null]
  implicit val stringPickler: Pickler[String] with Unpickler[String] = PrimitivePickler[String]
  implicit val unitPickler: Pickler[Unit] with Unpickler[Unit] = PrimitivePickler[Unit]
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
  def apply[A: FastTypeTag]: Pickler[A] with Unpickler[A] =
    new PrimitivePickler[A](FastTypeTag.valueTypeName(implicitly[FastTypeTag[A]]))
}
