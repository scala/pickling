package scala.pickling
package pickler

import java.math.BigInteger


/** This contains implicits which can serialize java.math.BigInteger values. */
trait JavaBigIntegerPicklers extends PrimitivePicklers {
  implicit val javaBigIntegerPickler:
    Pickler[BigInteger] with Unpickler[BigInteger] = new Pickler[BigInteger] with Unpickler[BigInteger] {
    def tag = FastTypeTag[BigInteger]
    def pickle(picklee: BigInteger, builder: PBuilder): Unit = {
      builder.beginEntry(picklee)

      builder.putField("value", b => {
        b.hintTag(implicitly[FastTypeTag[String]])
        b.hintStaticallyElidedType()
        stringPickler.pickle(picklee.toString, b)
      })

      builder.endEntry()
    }
    def unpickle(tag: String, reader: PReader): Any = {
      val reader1 = reader.readField("value")
      reader1.hintTag(implicitly[FastTypeTag[String]])
      reader1.hintStaticallyElidedType()

      val tag = reader1.beginEntry()
      val result = stringPickler.unpickle(tag, reader1)
      reader1.endEntry()

      new BigInteger(result.asInstanceOf[String])
    }
  } 
}
