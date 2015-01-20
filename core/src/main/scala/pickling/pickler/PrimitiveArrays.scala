package scala.pickling
package pickler

trait PrimitiveArrayPicklers {
  implicit val byteArrayPickler: SPickler[Array[Byte]] with Unpickler[Array[Byte]] = PrimitivePickler[Array[Byte]]
  implicit val shortArrayPickler: SPickler[Array[Short]] with Unpickler[Array[Short]] = PrimitivePickler[Array[Short]]
  implicit val charArrayPickler: SPickler[Array[Char]] with Unpickler[Array[Char]] = PrimitivePickler[Array[Char]]
  implicit val intArrayPickler: SPickler[Array[Int]] with Unpickler[Array[Int]] = PrimitivePickler[Array[Int]]
  implicit val longArrayPickler: SPickler[Array[Long]] with Unpickler[Array[Long]] = PrimitivePickler[Array[Long]]
  implicit val booleanArrayPickler: SPickler[Array[Boolean]] with Unpickler[Array[Boolean]] = PrimitivePickler[Array[Boolean]]
  implicit val floatArrayPickler: SPickler[Array[Float]] with Unpickler[Array[Float]] = PrimitivePickler[Array[Float]]
  implicit val doubleArrayPickler: SPickler[Array[Double]] with Unpickler[Array[Double]] = PrimitivePickler[Array[Double]]
}
