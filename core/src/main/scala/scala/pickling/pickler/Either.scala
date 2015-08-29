package scala.pickling
package pickler

/** Picklers for either trait. */
trait EitherPicklers {
  // TODO(jsuereth) - Register pickler generators

  implicit def pickleLeft[L, R](implicit lp: Pickler[L], t: FastTypeTag[Left[L,R]]): Pickler[Left[L, R]] =
     new AbstractPickler[Left[L, R]] {
       override def pickle(picklee: Left[L, R], builder: PBuilder): Unit = {
         builder.beginEntry(picklee, tag)
         if(lp.tag.isEffectivelyPrimitive) builder.hintElidedType(lp.tag)
         builder.putField("a", b => lp.pickle(picklee.a, b))
         builder.endEntry()
       }
       override def tag: FastTypeTag[Left[L, R]] = t
       override def toString = s"LeftPickler($tag)"
     }
  implicit def pickleRight[L,R](implicit rp: Pickler[R], t: FastTypeTag[Right[L,R]]): Pickler[Right[L,R]] =
    new AbstractPickler[Right[L, R]] {
      override def pickle(picklee: Right[L, R], builder: PBuilder): Unit = {
        builder.beginEntry(picklee, tag)
        if(rp.tag.isEffectivelyPrimitive) builder.hintElidedType(rp.tag)
        builder.putField("b", b => rp.pickle(picklee.b, b))
        builder.endEntry()
      }
      override def tag: FastTypeTag[Right[L, R]] = t
      override def toString = s"RightPickler($tag)"
    }

  implicit def pickleEither[L,R](implicit rp: Pickler[Right[L,R]], lp: Pickler[Left[L,R]], t: FastTypeTag[Either[L,R]]): Pickler[Either[L,R]] =
    new AbstractPickler[Either[L, R]] {
      override def pickle(picklee: Either[L, R], builder: PBuilder): Unit = {
        picklee match {
          case l: Left[L,R] => lp.pickle(l, builder)
          case r: Right[L,R] => rp.pickle(r, builder)
        }
      }
      override def tag: FastTypeTag[Either[L, R]] = t
      override def toString = s"EitherPickler($t)"
    }

  implicit def unpickleLeft[L, R](implicit lp: Unpickler[L], t: FastTypeTag[Left[L,R]]): Unpickler[Left[L, R]] =
    new AbstractUnpickler[Left[L, R]] {
      override def toString = s"LeftUnpickler($tag)"
      override def unpickle(tag: String, reader: PReader): Any = {
        // TODO - check tag == our tag?
        val rr = reader.readField("a")
        if(lp.tag.isEffectivelyPrimitive) rr.hintElidedType(lp.tag)
        Left(lp.unpickleEntry(rr).asInstanceOf[R])
      }
      override def tag: FastTypeTag[Left[L, R]] = t
    }
  implicit def unpickleRight[L, R](implicit rp: Unpickler[R], t: FastTypeTag[Right[L,R]]): Unpickler[Right[L, R]] =
    new AbstractUnpickler[Right[L, R]] {
      override def unpickle(tag: String, reader: PReader): Any = {
        // TODO - check tag == our tag?
        val rr = reader.readField("b")
        if(rp.tag.isEffectivelyPrimitive) rr.hintElidedType(rp.tag)
        Right(rp.unpickleEntry(rr).asInstanceOf[R])
      }
      override def tag: FastTypeTag[Right[L, R]] = t
      override def toString = s"RightUnpickler($tag)"
    }
  implicit def unpickleEither[L, R](implicit rp: Unpickler[Right[L,R]], lp: Unpickler[Left[L,R]],
                                   t: FastTypeTag[Either[L,R]]): Unpickler[Either[L, R]] =
    new AbstractUnpickler[Either[L, R]] {
      override def unpickle(tag: String, reader: PReader): Any = {
        if(tag == rp.tag.key) rp.unpickle(tag,reader)
          else if(tag == lp.tag.key) lp.unpickle(tag, reader)
          else throw new PicklingException(s"Unknown type tag for Either: $tag")
      }
      override def tag: FastTypeTag[Either[L, R]] = t
      override def toString = s"EitherUnpickler($tag)"
    }
}