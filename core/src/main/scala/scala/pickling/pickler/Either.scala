package scala.pickling
package pickler

import scala.pickling.PicklingErrors.TypeMismatch

/** Generate [[Pickler]]s and [[Unpickler]]s for [[Either]]
  * and its subclasses [[Right]] and [[Left]].
  */
trait EitherPicklers extends EitherPicklersRuntime with GeneratorRegistry {

  implicit def pickleUnpickleLeft[L, R]
    (implicit lp: Pickler[L], lu: Unpickler[L],
               t: FastTypeTag[Left[L,R]]): AbstractPicklerUnpickler[Left[L, R]] = {
    new AbstractPicklerUnpickler[Left[L, R]] with AutoRegister[Left[L, R]] {
      override lazy val tag: FastTypeTag[Left[L, R]] = t

      override def pickle(picklee: Left[L, R], builder: PBuilder): Unit = {
        builder.beginEntry(picklee, tag)
        if (lp.tag.isEffectivelyPrimitive) builder.hintElidedType(lp.tag)
        builder.putField("a", b => lp.pickle(picklee.a, b))
        builder.endEntry()
      }

      override def unpickle(tag: String, reader: PReader): Any = {
        if (t.key == tag) {
          val rr = reader.readField("a")
          if (lp.tag.isEffectivelyPrimitive) rr.hintElidedType(lp.tag)
          Left(lu.unpickleEntry(rr).asInstanceOf[R])
        } else throw TypeMismatch(List(t), List(FastTypeTag(tag)))
      }

      override def toString = s"LeftPicklerUnpickler($tag)"
    }
  }

  implicit def pickleUnpickleRight[L,R]
    (implicit rp: Pickler[R], ru: Unpickler[R],
               t: FastTypeTag[Right[L,R]]): AbstractPicklerUnpickler[Right[L,R]] = {
    new AbstractPicklerUnpickler[Right[L, R]] with AutoRegister[Right[L, R]] {
      override lazy val tag: FastTypeTag[Right[L, R]] = t

      override def pickle(picklee: Right[L, R], builder: PBuilder): Unit = {
        builder.beginEntry(picklee, tag)
        if (rp.tag.isEffectivelyPrimitive) builder.hintElidedType(rp.tag)
        builder.putField("b", b => rp.pickle(picklee.b, b))
        builder.endEntry()
      }

      override def unpickle(tag: String, reader: PReader): Any = {
        if (t.key == tag) {
          val rr = reader.readField("b")
          if (rp.tag.isEffectivelyPrimitive) rr.hintElidedType(rp.tag)
          Right(ru.unpickleEntry(rr).asInstanceOf[R])
        } else throw TypeMismatch(List(t), List(FastTypeTag(tag)))
      }

      override def toString = s"RightPicklerUnpickler($tag)"
    }
  }

  implicit def pickleUnpickleEither[L,R]
    (implicit rp: Pickler[Right[L,R]], ru: Unpickler[Right[L, R]],
              lp: Pickler[Left[L,R]], lu: Unpickler[Left[L, R]],
              t: FastTypeTag[Either[L,R]]): AbstractPicklerUnpickler[Either[L,R]] = {
    new AbstractPicklerUnpickler[Either[L, R]] with AutoRegister[Either[L, R]] {
      override def pickle(picklee: Either[L, R], builder: PBuilder): Unit = {
        picklee match {
          case l: Left[L, R] => lp.pickle(l, builder)
          case r: Right[L, R] => rp.pickle(r, builder)
        }
      }

      override def unpickle(tag: String, reader: PReader): Any = {
        if (tag == rp.tag.key) ru.unpickle(tag, reader)
        else if (tag == lp.tag.key) lu.unpickle(tag, reader)
        else throw TypeMismatch(List(rp.tag, lp.tag), List(FastTypeTag(tag)))
      }

      override def tag: FastTypeTag[Either[L, R]] = t

      override def toString = s"EitherPicklerUnpickler($tag)"
    }
  }

  locally {

    registerPicklerAsGen(RuntimeLeftPicklerUnpickler)
    registerPicklerAsGen(RuntimeRightPicklerUnpickler)
    registerPicklerAsGen(RuntimeEitherPicklerUnpickler)

  }

}

trait EitherPicklersRuntime extends GeneratorHelper {

  /** Pickle as [[Any]] because the scala pickling
    * use [[Any]] as a placeholder for the type params.
    */
  private def pickleAsAny[S <: Either[_, _]]
      (picklee: S, fullTpe: FastTypeTag[S],
       name: String, field: Any, builder: PBuilder) = {

    builder.beginEntry(picklee, fullTpe)
    builder.putField(name, { b =>
      AnyPicklerUnpickler.pickle(field, b)
    })
    builder.endEntry()

  }

  /** Unpickle with a given unpickler a concrete field. */
  private def unpickleAsAny[T](unpickler: Unpickler[T],
                               reader: PReader, name: String) = {
    val field = reader.readField(name)
    unpickler.unpickleEntry(field)
  }


  /** Custom runtime [[Pickler]] and [[Unpickler]] generator of [[Left]]. */
  object RuntimeLeftPicklerUnpickler
    extends AbstractPicklerUnpickler[Left[Any, Any]] {

    val tag = FastTypeTag[Left[Any, Any]]("scala.util.Left[scala.Any,scala.Any]")

    def pickle(picklee: Left[Any, Any], builder: PBuilder): Unit =
      pickleAsAny(picklee, tag, "a", picklee.a, builder)

    def unpickle(tagTpe: String, reader: PReader): Any = {

      val tpe = FastTypeTag.apply(tagTpe)
      val (leftTpe, _) = twoArgumentTagExtractor[Any, Any](tpe)
      Left(unpickleAsAny(getUnpickler(leftTpe, tag), reader, "a"))

    }

  }

  /** Custom runtime [[Pickler]] and [[Unpickler]] generator of [[Right]]. */
  object RuntimeRightPicklerUnpickler
    extends AbstractPicklerUnpickler[Right[Any, Any]] {

    val tag = FastTypeTag[Right[Any, Any]]("scala.util.Right[scala.Any,scala.Any]")

    def pickle(picklee: Right[Any, Any], builder: PBuilder): Unit =
      pickleAsAny(picklee, tag, "b", picklee.b, builder)

    def unpickle(tagTpe: String, reader: PReader): Any = {

      val tpe = FastTypeTag.apply(tagTpe)
      val (_, rightTpe) = twoArgumentTagExtractor[Any, Any](tpe)
      Right(unpickleAsAny(getUnpickler(rightTpe, tag), reader, "b"))

    }

  }
  /** Custom runtime [[Pickler]] and [[Unpickler]] generator of [[Either]]. */
  object RuntimeEitherPicklerUnpickler
    extends AbstractPicklerUnpickler[Either[Any, Any]] {

    val tag = FastTypeTag[Either[Any, Any]]("scala.util.Either[scala.Any,scala.Any]")

    def pickle(picklee: Either[Any, Any], builder: PBuilder): Unit = {
      picklee match {
        case r: Right[Any, Any] =>
          RuntimeRightPicklerUnpickler.pickle(r, builder)
        case l: Left[Any, Any] =>
          RuntimeLeftPicklerUnpickler.pickle(l, builder)
      }
    }

    def unpickle(tag: String, reader: PReader): Any = {
      // Tag is expected to be a concrete subclass of Either
      AnyPicklerUnpickler.unpickle(tag, reader)
    }

  }

}

