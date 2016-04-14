package scala.pickling
package pickler

import java.util.UUID

trait JavaUUIDPicklers extends PrimitivePicklers {

  implicit val javaUUIDPickler: Pickler[UUID] with Unpickler[UUID] =
    new AbstractPicklerUnpickler[UUID] with AutoRegister[UUID] {
      lazy val tag = FastTypeTag[UUID]("java.util.UUID")
      def pickle(picklee: java.util.UUID, builder: PBuilder):Unit = {
        builder.beginEntry(picklee, tag)

        builder.putField("msb", { b =>
          b.hintElidedType(FastTypeTag.Long)
          longPickler.pickle(picklee.getMostSignificantBits, b)
        })
        builder.putField("lsb", { b =>
          b.hintElidedType(FastTypeTag.Long)
          longPickler.pickle(picklee.getLeastSignificantBits, b)
        })
        builder.endEntry()
      }

      def unpickle(tag: String, reader: PReader): Any = {
        reader.hintElidedType(FastTypeTag.Long)
        reader.pinHints()
        val r1 = reader.readField("msb")
        val tag1 = r1.beginEntry()
        val msb = longPickler.unpickle(tag1, r1).asInstanceOf[Long]
        r1.endEntry()
        val r2 = reader.readField("lsb")
        val tag2 = r2.beginEntry()
        val lsb = longPickler.unpickle(tag2, r2).asInstanceOf[Long]
        r2.endEntry()
        reader.unpinHints()
        new java.util.UUID(msb, lsb)
      }
    }
  internal.currentRuntime.picklers.registerPicklerUnpickler(javaUUIDPickler)
}
