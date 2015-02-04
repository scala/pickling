package scala.pickling
package pickler

import java.math.BigDecimal

/** Contains picklers which serialize java.math.BigDecimal.
  * Note; This currently serialzies as a string.
  */
trait JavaBigDecimalPicklers extends PrimitivePicklers {
  implicit val javaBigDecimalPickler:
    Pickler[BigDecimal] with Unpickler[BigDecimal] = new Pickler[BigDecimal] with Unpickler[BigDecimal]{
    def tag = FastTypeTag[BigDecimal]
    def pickle(picklee: BigDecimal, builder: PBuilder): Unit = {
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

      new BigDecimal(result.asInstanceOf[String])
    }
  }
}
