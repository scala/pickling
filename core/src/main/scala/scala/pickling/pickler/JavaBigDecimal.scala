package scala.pickling
package pickler

import java.math.BigDecimal

/** Contains picklers which serialize java.math.BigDecimal.
  * Note; This currently serialzies as a string.
  */
trait JavaBigDecimalPicklers extends PrimitivePicklers {
  implicit val javaBigDecimalPickler: Pickler[BigDecimal] with Unpickler[BigDecimal] =
    new AbstractPicklerUnpickler[BigDecimal] with AutoRegister[BigDecimal] {
      lazy val tag = FastTypeTag[BigDecimal]("java.math.BigDecimal")
      def pickle(picklee: BigDecimal, builder: PBuilder): Unit = {
        builder.beginEntry(picklee, tag)
        builder.putField("value", b => {
          b.hintElidedType(implicitly[FastTypeTag[String]])
          stringPickler.pickle(picklee.toString, b)
        })
        builder.endEntry()
      }
      def unpickle(tag: String, reader: PReader): Any = {
        val reader1 = reader.readField("value")
        reader1.hintElidedType(implicitly[FastTypeTag[String]])
        val result = stringPickler.unpickleEntry(reader1)
        new BigDecimal(result.asInstanceOf[String])
      }
    }
  internal.currentRuntime.picklers.registerPicklerUnpickler(javaBigDecimalPickler)
}
