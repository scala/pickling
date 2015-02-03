package scala

package object pickling {

  /** A combination of a static pickler and an unpickler for type `T`.
   */
  type SPicklerUnpickler[T] = SPickler[T] with Unpickler[T]
  object SPicklerUnpickler {
    import scala.language.experimental.macros

    // these return the two traits rather than the alias just in case
    // the alias would somehow have a different meaning in some context

    def apply[T](p: SPickler[T], u: Unpickler[T]): SPickler[T] with Unpickler[T] = new SPicklerUnpicklerImpl(p, u)
    def generate[T]: SPickler[T] with Unpickler[T] = macro Compat.SpicklerUnpicklerMacros_impl[T]
    /** This is a private implementation of SPicklerUnpickler that delegates pickle and unpickle to underlying. */
    private class SPicklerUnpicklerImpl[T](p: SPickler[T], u: Unpickler[T]) extends SPickler[T] with Unpickler[T] {
      // From SPickler
      override def pickle(picklee: T, builder: PBuilder): Unit = p.pickle(picklee, builder)
      // From SPickler and Unpickler
      override def tag: FastTypeTag[T] = p.tag
      // From Unpickler
      override def unpickle(tag: String, reader: PReader): Any = u.unpickle(tag, reader)
    }
  }
}
