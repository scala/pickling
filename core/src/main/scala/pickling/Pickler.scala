package scala.pickling

import scala.language.experimental.macros

import scala.annotation.implicitNotFound
import scala.reflect.runtime.universe._

/** A static pickler for type `T`. Its `pickle` method takes an object-to-be-pickled of
 *  static type `T`, and pickles it to an instance of `PBuilder`. In the process the object
 *  is turned into some external representation like a byte array. The particular external
 *  representation (the "pickle format") is defined by the `format` val member (of type
 *  `PickleFormat`).
 *
 *  This pickler requires that the dynamic type of the object-to-be-pickled is equal to
 *  the erasure of its static type `T`.
 */
@implicitNotFound(msg = "Cannot generate a pickler for ${T}. Recompile with -Xlog-implicits for details")
trait SPickler[T] {
  val format: PickleFormat
  def pickle(picklee: T, builder: PBuilder): Unit
}

/** A dynamic pickler for type `T`. Its `pickle` method takes an object-to-be-pickled of
 *  static type `T`, and pickles it to an instance of `PBuilder`. In the process the object
 *  is turned into some external representation like a byte array. The particular external
 *  representation (the "pickle format") is defined by the `format` val member (of type
 *  `PickleFormat`).
 *
 *  In contrast to static picklers (instances of type `SPickler[T]`), a dynamic pickler of
 *  type `DPickler[T]` pickles any object of type `T`.
 */
@implicitNotFound(msg = "Cannot generate a DPickler for ${T}. Recompile with -Xlog-implicits for details")
trait DPickler[T] {
  val format: PickleFormat
  def pickle(picklee: T, builder: PBuilder): Unit
}

object DPickler {
  implicit def genDPickler[T](implicit format: PickleFormat): DPickler[T] = macro Compat.PicklerMacros_dpicklerImpl[T]
}

trait GenPicklers {
  implicit def genPickler[T](implicit format: PickleFormat): SPickler[T] = macro Compat.PicklerMacros_impl[T]
  // TODO: the primitive pickler hack employed here is funny, but I think we should fix this one
  // since people probably would also have to deal with the necessity to abstract over pickle formats
  def genPickler(classLoader: ClassLoader, clazz: Class[_])(implicit format: PickleFormat, share: refs.Share): SPickler[_] = {
    // println(s"generating runtime pickler for $clazz") // NOTE: needs to be an explicit println, so that we don't occasionally fallback to runtime in static cases
    //val runtime = new CompiledPicklerRuntime(classLoader, clazz)
    val runtime = new InterpretedPicklerRuntime(classLoader, clazz)
    runtime.genPickler
  }
}

// marker trait to indicate a generated pickler
// this is important for the dispatch between custom and generated picklers
trait Generated

object SPickler extends CorePicklersUnpicklers

@implicitNotFound(msg = "Cannot generate an unpickler for ${T}. Recompile with -Xlog-implicits for details")
trait Unpickler[T] {
  val format: PickleFormat
  def unpickle(tag: => FastTypeTag[_], reader: PReader): Any
}

trait GenUnpicklers {
  implicit def genUnpickler[T](implicit format: PickleFormat): Unpickler[T] = macro Compat.UnpicklerMacros_impl[T]
  def genUnpickler(mirror: Mirror, tag: FastTypeTag[_])(implicit format: PickleFormat, share: refs.Share): Unpickler[_] = {
    // println(s"generating runtime unpickler for ${tag.tpe}") // NOTE: needs to be an explicit println, so that we don't occasionally fallback to runtime in static cases
    //val runtime = new CompiledUnpicklerRuntime(mirror, tag.tpe)
    val runtime = new InterpretedUnpicklerRuntime(mirror, tag)
    runtime.genUnpickler
  }
}

object Unpickler extends CorePicklersUnpicklers
