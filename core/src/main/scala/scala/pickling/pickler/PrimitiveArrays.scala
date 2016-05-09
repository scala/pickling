package scala.pickling
package pickler

/** Picklers for primitive arrays.
 */
trait PrimitiveArrayPicklers {
  implicit val byteArrayPickler: AbstractPicklerUnpickler[Array[Byte]] = PrimitivePickler[Array[Byte]]
  implicit val shortArrayPickler: AbstractPicklerUnpickler[Array[Short]] = PrimitivePickler[Array[Short]]
  implicit val charArrayPickler: AbstractPicklerUnpickler[Array[Char]] = PrimitivePickler[Array[Char]]
  implicit val intArrayPickler: AbstractPicklerUnpickler[Array[Int]] = PrimitivePickler[Array[Int]]
  implicit val longArrayPickler: AbstractPicklerUnpickler[Array[Long]] = PrimitivePickler[Array[Long]]
  implicit val booleanArrayPickler: AbstractPicklerUnpickler[Array[Boolean]] = PrimitivePickler[Array[Boolean]]
  implicit val floatArrayPickler: AbstractPicklerUnpickler[Array[Float]] = PrimitivePickler[Array[Float]]
  implicit val doubleArrayPickler: AbstractPicklerUnpickler[Array[Double]] with Unpickler[Array[Double]] = PrimitivePickler[Array[Double]]
}
