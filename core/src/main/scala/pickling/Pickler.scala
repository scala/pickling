package scala.pickling

import scala.language.experimental.macros

import scala.annotation.implicitNotFound
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.universe._

import internal._

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

trait GenPicklers extends CorePicklersUnpicklers {

  implicit def genPickler[T](implicit format: PickleFormat): SPickler[T] = macro Compat.PicklerMacros_impl[T]
  // TODO: the primitive pickler hack employed here is funny, but I think we should fix this one
  // since people probably would also have to deal with the necessity to abstract over pickle formats
  def genPickler(classLoader: ClassLoader, clazz: Class[_], tag: FastTypeTag[_])(implicit format: PickleFormat, share: refs.Share): SPickler[_] = {
    // println(s"generating runtime pickler for $clazz") // NOTE: needs to be an explicit println, so that we don't occasionally fallback to runtime in static cases
    val className = if (clazz == null) "null" else clazz.getName
    GlobalRegistry.picklerMap.get(className) match {
      case None =>
        // debug(s"!!! could not find registered pickler for class $className !!!")
        val pickler: SPickler[_] = if (clazz.isArray) {
          val mirror = ru.runtimeMirror(classLoader)
          val elemClass = clazz.getComponentType()
          val elemTag = FastTypeTag.mkRaw(elemClass, mirror)
          val elemPickler = genPickler(classLoader, elemClass, elemTag)

          mkRuntimeTravPickler[Array[AnyRef]](mirror, elemClass, elemTag, tag, elemPickler, null)
        } else {
          val runtime = new RuntimePickler(classLoader, clazz)
          runtime.mkPickler
        }
        GlobalRegistry.picklerMap += (className -> pickler)
        pickler

      case Some(existingPickler) =>
        existingPickler
    }
  }
}

// marker trait to indicate a generated pickler
// this is important for the dispatch between custom and generated picklers
trait Generated

object SPickler extends GenPicklers

@implicitNotFound(msg = "Cannot generate an unpickler for ${T}. Recompile with -Xlog-implicits for details")
trait Unpickler[T] {
  val format: PickleFormat
  def unpickle(tag: => FastTypeTag[_], reader: PReader): Any
}

trait GenUnpicklers extends CorePicklersUnpicklers {
  implicit def genUnpickler[T](implicit format: PickleFormat): Unpickler[T] with Generated = macro Compat.UnpicklerMacros_impl[T]
  def genUnpickler(mirror: Mirror, tag: FastTypeTag[_])(implicit format: PickleFormat, share: refs.Share): Unpickler[_] = {
    // println(s"generating runtime unpickler for ${tag.key}") // NOTE: needs to be an explicit println, so that we don't occasionally fallback to runtime in static cases
    val className = tag.key
    GlobalRegistry.unpicklerMap.get(className) match {
      case None =>
        // debug(s"!!! could not find registered pickler for class $className !!!")
        val unpickler = if (className.startsWith("scala.Array")) {
          // debug(s"runtime unpickling of an array: $className")
          val len = className.length
          val elemTypeString = className.substring(12, len-1)
          // debug(s"creating tag for element type: $elemTypeString")
          val elemTag = FastTypeTag(mirror, elemTypeString)
          val elemClass = Class.forName(elemTypeString)
          val elemUnpickler = Unpickler.genUnpickler(mirror, elemTag)

          mkRuntimeTravPickler[Array[AnyRef]](mirror, elemClass, elemTag, tag, null, elemUnpickler)
        } else {
          val runtime = new InterpretedUnpicklerRuntime(mirror, tag)
          runtime.genUnpickler
        }
        GlobalRegistry.unpicklerMap += (className -> unpickler)
        unpickler
      case Some(existingUnpickler) =>
        existingUnpickler
    }
  }
}

object Unpickler extends GenUnpicklers
