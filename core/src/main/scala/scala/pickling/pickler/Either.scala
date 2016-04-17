package scala.pickling
package pickler

/** Generate [[Pickler]]s and [[Unpickler]]s for [[Either]]
  * and its subclasses [[Right]] and [[Left]].
  */
trait EitherPicklers {
  // TODO(jsuereth) - Register pickler generators

  implicit def pickleUnpickleLeft[L, R](implicit lp: Pickler[L], lu: Unpickler[L],
                                        t: FastTypeTag[Left[L,R]]): AbstractPicklerUnpickler[Left[L, R]] =
     new AbstractPicklerUnpickler[Left[L, R]] with AutoRegister[Left[L, R]] {
       override lazy val tag: FastTypeTag[Left[L, R]] = t
       override def pickle(picklee: Left[L, R], builder: PBuilder): Unit = {
         builder.beginEntry(picklee, tag)
         if(lp.tag.isEffectivelyPrimitive) builder.hintElidedType(lp.tag)
         builder.putField("a", b => lp.pickle(picklee.a, b))
         builder.endEntry()
       }
       override def unpickle(tag: String, reader: PReader): Any = {
         if (t.key == tag) {
           val rr = reader.readField("a")
           if(lp.tag.isEffectivelyPrimitive) rr.hintElidedType(lp.tag)
           Left(lu.unpickleEntry(rr).asInstanceOf[R])
         } else throw new PicklingException(s"LeftUnpickler can't unpickle: $tag")
       }
       override def toString = s"LeftPicklerUnpickler($tag)"
     }

  implicit def pickleUnpickleRight[L,R](implicit rp: Pickler[R], ru: Unpickler[R],
                                        t: FastTypeTag[Right[L,R]]): AbstractPicklerUnpickler[Right[L,R]] =
    new AbstractPicklerUnpickler[Right[L, R]] with AutoRegister[Right[L, R]] {
      override lazy val tag: FastTypeTag[Right[L, R]] = t
      override def pickle(picklee: Right[L, R], builder: PBuilder): Unit = {
        builder.beginEntry(picklee, tag)
        if(rp.tag.isEffectivelyPrimitive) builder.hintElidedType(rp.tag)
        builder.putField("b", b => rp.pickle(picklee.b, b))
        builder.endEntry()
      }
      override def unpickle(tag: String, reader: PReader): Any = {
        if (t.key == tag) {
          val rr = reader.readField("b")
          if(rp.tag.isEffectivelyPrimitive) rr.hintElidedType(rp.tag)
          Right(ru.unpickleEntry(rr).asInstanceOf[R])
        } else throw new PicklingException(s"RightUnpickler can't unpickle: $tag")
      }
      override def toString = s"RightPicklerUnpickler($tag)"
    }

  implicit def pickleUnpickleEither[L,R](implicit rp: Pickler[Right[L,R]], ru: Unpickler[Right[L, R]],
                                         lp: Pickler[Left[L,R]], lu: Unpickler[Left[L, R]],
                                         t: FastTypeTag[Either[L,R]]): AbstractPicklerUnpickler[Either[L,R]] =
    new AbstractPicklerUnpickler[Either[L, R]] with AutoRegister[Either[L, R]] {
      override def pickle(picklee: Either[L, R], builder: PBuilder): Unit = {
        picklee match {
          case l: Left[L,R] => lp.pickle(l, builder)
          case r: Right[L,R] => rp.pickle(r, builder)
        }
      }
      override def unpickle(tag: String, reader: PReader): Any = {
        if(tag == rp.tag.key) ru.unpickle(tag,reader)
        else if(tag == lp.tag.key) lu.unpickle(tag, reader)
        else throw new PicklingException(s"Unknown type tag for Either: $tag")
      }
      override def tag: FastTypeTag[Either[L, R]] = t
      override def toString = s"EitherPicklerUnpickler($t)"
    }

}