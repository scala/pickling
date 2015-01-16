package scala.pickling

import java.math.BigInteger

trait JavaBigIntegerPicklers {
  implicit def javaBigIntegerPickler(implicit sp: SPickler[String] with Unpickler[String]):
    SPickler[BigInteger] with Unpickler[BigInteger] = new SPickler[BigInteger] with Unpickler[BigInteger] {
    def tag = FastTypeTag[BigInteger]
    def pickle(picklee: BigInteger, builder: PBuilder): Unit = {
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

      new BigInteger(result.asInstanceOf[String])
    }
  } 
}

object JavaBigIntegerPicklers extends JavaBigIntegerPicklers {}
