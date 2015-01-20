package scala.pickling
package pickler

trait PrimitivePicklers {
  // TODO: figure out why removing these pickler/unpicklers slows down evactor1
  implicit val bytePickler: SPickler[Byte] with Unpickler[Byte] = PrimitivePickler[Byte]
  implicit val shortPickler: SPickler[Short] with Unpickler[Short] = PrimitivePickler[Short]
  implicit val charPickler: SPickler[Char] with Unpickler[Char] = PrimitivePickler[Char]
  implicit val intPickler: SPickler[Int] with Unpickler[Int] = PrimitivePickler[Int]
  implicit val longPickler: SPickler[Long] with Unpickler[Long] = PrimitivePickler[Long]
  implicit val booleanPickler: SPickler[Boolean] with Unpickler[Boolean] = PrimitivePickler[Boolean]
  implicit val floatPickler: SPickler[Float] with Unpickler[Float] = PrimitivePickler[Float]
  implicit val doublePickler: SPickler[Double] with Unpickler[Double] = PrimitivePickler[Double]
  implicit val nullPickler: SPickler[Null] with Unpickler[Null] = PrimitivePickler[Null]
  implicit val stringPickler: SPickler[String] with Unpickler[String] = PrimitivePickler[String]
  implicit val unitPickler: SPickler[Unit] with Unpickler[Unit] = PrimitivePickler[Unit]
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
  def apply[A: FastTypeTag]: SPickler[A] with Unpickler[A] =
    new PrimitivePickler[A](FastTypeTag.valueTypeName(implicitly[FastTypeTag[A]]))
}
