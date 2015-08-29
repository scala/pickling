package scala.pickling
package pickler

import java.math.BigInteger


/** This contains implicits which can serialize java.math.BigInteger values. */
trait JavaBigIntegerPicklers extends PrimitivePicklers {
  // TODO(jsuereth) - Register runtime picklers
  implicit val javaBigIntegerPickler:
    Pickler[BigInteger] with Unpickler[BigInteger] = new AbstractPicklerUnpickler[BigInteger] {
    def tag = FastTypeTag[BigInteger]
    def pickle(picklee: BigInteger, builder: PBuilder): Unit = {
      builder.beginEntry(picklee, tag)

      builder.putField("value", b => {
        b.hintElidedType(FastTypeTag[String])
        stringPickler.pickle(picklee.toString, b)
      })

      builder.endEntry()
    }
    def unpickle(tag: String, reader: PReader): Any = {
      val reader1 = reader.readField("value")
      reader1.hintElidedType(implicitly[FastTypeTag[String]])
      val result = stringPickler.unpickleEntry(reader1)
      new BigInteger(result.asInstanceOf[String])
    }
  }

  // TODO - Figure out if we should somehow have these all registered somewhere else rather than take the hit at construction time.
  internal.currentRuntime.picklers.registerPicklerUnpickler(javaBigIntegerPickler.tag.key, javaBigIntegerPickler)
}
