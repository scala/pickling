package scala.pickling
package pickler

import java.math.BigInteger


/** This contains implicits which can serialize java.math.BigInteger values. */
trait JavaBigIntegerPicklers extends PrimitivePicklers {
  implicit val javaBigIntegerPickler: Pickler[BigInteger] with Unpickler[BigInteger] =
    new AbstractPicklerUnpickler[BigInteger] with AutoRegister[BigInteger] {
      lazy val tag = FastTypeTag[BigInteger]("java.math.BigInteger")
      def pickle(picklee: BigInteger, builder: PBuilder): Unit = {
        builder.beginEntry(picklee, tag)

        builder.putField("value", b => {
          b.hintElidedType(FastTypeTag.String)
          stringPickler.pickle(picklee.toString, b)
        })

        builder.endEntry()
      }
      def unpickle(tag: String, reader: PReader): Any = {
        val reader1 = reader.readField("value")
        reader1.hintElidedType(FastTypeTag.String)
        val result = stringPickler.unpickleEntry(reader1)
        new BigInteger(result.asInstanceOf[String])
      }
    }
  internal.currentRuntime.picklers.registerPicklerUnpickler(javaBigIntegerPickler)
}
