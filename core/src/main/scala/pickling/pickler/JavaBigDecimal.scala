package scala.pickling

import java.math.BigDecimal

trait JavaBigDecimalPicklers {
  implicit def javaBigDecimalPickler(implicit sp: SPickler[String] with Unpickler[String]): 
    SPickler[BigDecimal] with Unpickler[BigDecimal] = new SPickler[BigDecimal] with Unpickler[BigDecimal]{
    def tag = FastTypeTag[BigDecimal]
    def pickle(picklee: BigDecimal, builder: PBuilder): Unit = {
      builder.beginEntry(picklee)

      builder.putField("value", b => {
        b.hintTag(implicitly[FastTypeTag[String]])
        b.hintStaticallyElidedType()
        sp.pickle(picklee.toString, b)
      })

      builder.endEntry()
    }
    def unpickle(tag: String, reader: PReader): Any = {
      val reader1 = reader.readField("value")
      reader1.hintTag(implicitly[FastTypeTag[String]])
      reader1.hintStaticallyElidedType()

      val tag = reader1.beginEntry()
      val result = sp.unpickle(tag, reader1)
      reader1.endEntry()

      new BigDecimal(result.asInstanceOf[String])
    }
  }
}

object JavaBigDecimalPicklers extends JavaBigDecimalPicklers {}
