package scala.pickling
package pickler

/**
 * Pickler implicits for type tags.
 */
trait TypeTagPicklers extends PrimitivePicklers {
  implicit def typeTagPickler[T]: AbstractPicklerUnpickler[FastTypeTag[T]] =
    FastTypeTagPicklerUnpickler.asInstanceOf[AbstractPicklerUnpickler[FastTypeTag[T]]]


  private[pickler] object FastTypeTagPicklerUnpickler
    extends AbstractPicklerUnpickler[FastTypeTag[_]] with AutoRegister[FastTypeTag[_]] {
    override def pickle(picklee: FastTypeTag[_], builder: PBuilder): Unit = {
      builder.beginEntry(picklee, tag)
      builder.putField("key", { b =>
        b.hintElidedType(stringPickler.tag)
        stringPickler.pickle(picklee.key, b)
      })
      builder.endEntry()
    }

    /** Unpickles an entry out of the reader.
      *
      * note:  This method ASSUMES beginEntry() has already been called and endEntry() will be
      * called immediately afterwards.
      *
      * @param tag  The FastTypeTag[_].key that was serialized with the entry *or* the type hint
      *             which was provided when reading.  This is generally used by abstract type
      *             Unpicklers to delegate to the appropriate concrete unpickler.
      * @param reader  The reader we can grab fields, primitives or collection items out of.
      * @return Any an instance of the type we've unpickled.
      */
    override def unpickle(tag: String, reader: PReader): Any = {
      val rk = reader.readField("key")
      rk.hintElidedType(stringPickler.tag)
      val key = stringPickler.unpickleEntry(rk).toString
      FastTypeTag.apply(key)
    }
    override lazy val tag: FastTypeTag[FastTypeTag[_]] = FastTypeTag.apply("scala.pickling.pickler.FastTypeTag").asInstanceOf[FastTypeTag[FastTypeTag[_]]]

  }
  // Ensure we register for runtime deserialization.
  internal.currentRuntime.picklers.registerPicklerUnpicklerGenerator("scala.pickling.pickler.FastTypeTag", generator = ignore => FastTypeTagPicklerUnpickler)
}

