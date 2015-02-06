package scala.pickling
package pickler

/** Picklers for primitive arrays.
 */
trait PrimitiveArrayPicklers {
  implicit val byteArrayPickler: Pickler[Array[Byte]] with Unpickler[Array[Byte]] = PrimitivePickler[Array[Byte]]
  implicit val shortArrayPickler: Pickler[Array[Short]] with Unpickler[Array[Short]] = PrimitivePickler[Array[Short]]
  implicit val charArrayPickler: Pickler[Array[Char]] with Unpickler[Array[Char]] = PrimitivePickler[Array[Char]]
  implicit val intArrayPickler: Pickler[Array[Int]] with Unpickler[Array[Int]] = PrimitivePickler[Array[Int]]
  implicit val longArrayPickler: Pickler[Array[Long]] with Unpickler[Array[Long]] = PrimitivePickler[Array[Long]]
  implicit val booleanArrayPickler: Pickler[Array[Boolean]] with Unpickler[Array[Boolean]] = PrimitivePickler[Array[Boolean]]
  implicit val floatArrayPickler: Pickler[Array[Float]] with Unpickler[Array[Float]] = PrimitivePickler[Array[Float]]
  implicit val doubleArrayPickler: Pickler[Array[Double]] with Unpickler[Array[Double]] = PrimitivePickler[Array[Double]]
}
