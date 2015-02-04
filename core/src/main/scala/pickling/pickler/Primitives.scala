package scala.pickling
package pickler

/** Picklers for primitive types.
 */
trait PrimitivePicklers {
  // TODO: figure out why removing these pickler/unpicklers slows down evactor1
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

class PrimitivePickler[T: FastTypeTag](name: String) extends AutoRegister[T](name) {
  def pickle(picklee: T, builder: PBuilder): Unit = {
    builder.beginEntry(picklee)
    builder.endEntry()
  }
  def unpickle(tag: String, reader: PReader): Any = {
    try {
      reader.readPrimitive()
    } catch {
      case PicklingException(msg, cause) =>
        throw PicklingException(s"""error in unpickle of primitive unpickler '$name':
                                   |tag in unpickle: '${tag}'
                                   |message:
                                   |$msg""".stripMargin, cause)
    }
  }
}
object PrimitivePickler {
  def apply[A: FastTypeTag]: Pickler[A] with Unpickler[A] =
    new PrimitivePickler[A](FastTypeTag.valueTypeName(implicitly[FastTypeTag[A]]))
}
