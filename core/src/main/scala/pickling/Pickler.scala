package scala.pickling

import scala.language.experimental.macros

import scala.annotation.implicitNotFound


/** A static pickler for type `T`. Its `pickle` method takes an object-to-be-pickled of
 *  static type `T`, and pickles it to an instance of `PBuilder`. In the process the object
 *  is turned into some external representation like a byte array. The particular external
 *  representation (the "pickle format") is defined by the `builder`.
 *
 *  This pickler requires that the dynamic type of the object-to-be-pickled is equal to
 *  the erasure of its static type `T`.
 */
@implicitNotFound(msg = "Cannot generate a pickler for ${T}. Recompile with -Xlog-implicits for details")
trait SPickler[T] {
  def pickle(picklee: T, builder: PBuilder): Unit
  def tag: FastTypeTag[T]
}

/** A dynamic pickler for type `T`. Its `pickle` method takes an object-to-be-pickled of
 *  static type `T`, and pickles it to an instance of `PBuilder`. In the process the object
 *  is turned into some external representation like a byte array. The particular external
 *  representation (the "pickle format") is defined by the `builder`.
 *
 *  In contrast to static picklers (instances of type `SPickler[T]`), a dynamic pickler of
 *  type `DPickler[T]` pickles any object of type `T`.
 */
@implicitNotFound(msg = "Cannot generate a DPickler for ${T}. Recompile with -Xlog-implicits for details")
trait DPickler[T] {
  def pickle(picklee: T, builder: PBuilder): Unit
}

object DPickler {
  implicit def genDPickler[T]: DPickler[T] = macro Compat.PicklerMacros_dpicklerImpl[T]
}

trait GenPicklers {
  implicit def genPickler[T]: SPickler[T] = macro Compat.PicklerMacros_impl[T]
}

// marker trait to indicate a generated pickler
// this is important for the dispatch between custom and generated picklers
trait Generated

@implicitNotFound(msg = "Cannot generate an unpickler for ${T}. Recompile with -Xlog-implicits for details")
trait Unpickler[T] {
  def unpickle(tag: => FastTypeTag[_], reader: PReader): Any
  def tag: FastTypeTag[T]
}

trait GenOpenSumUnpicklers {
  implicit def genOpenSumUnpickler[T]: Unpickler[T] with Generated = macro Compat.OpenSumUnpicklerMacro_impl[T]
}

trait GenUnpicklers extends GenOpenSumUnpicklers {
  implicit def genUnpickler[T]: Unpickler[T] with Generated = macro Compat.UnpicklerMacros_impl[T]
}
