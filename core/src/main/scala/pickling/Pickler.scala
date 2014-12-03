package scala.pickling

import scala.language.experimental.macros

import scala.annotation.implicitNotFound
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.universe._

import internal._

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

trait GenPicklers extends RuntimePicklersUnpicklers {

  implicit def genPickler[T]: SPickler[T] = macro Compat.PicklerMacros_impl[T]
  // TODO: the primitive pickler hack employed here is funny, but I think we should fix this one
  // since people probably would also have to deal with the necessity to abstract over pickle formats
  def genPickler(classLoader: ClassLoader, clazz: Class[_], tag: FastTypeTag[_])(implicit share: refs.Share): SPickler[_] = {
    // println(s"generating runtime pickler for $clazz") // NOTE: needs to be an explicit println, so that we don't occasionally fallback to runtime in static cases
    val className = if (clazz == null) "null" else clazz.getName
    GlobalRegistry.picklerMap.get(className) match {
      case None =>
        // debug(s"!!! could not find registered pickler for class $className, tag ${tag.key} !!!")
        val pickler: SPickler[_] = if (clazz.isArray) {
          val mirror = ru.runtimeMirror(classLoader)
          val elemClass = clazz.getComponentType()
          val elemTag = FastTypeTag.mkRaw(elemClass, mirror)
          val elemPickler = genPickler(classLoader, elemClass, elemTag)

          mkRuntimeTravPickler[Array[AnyRef]](elemClass, elemTag, tag, elemPickler, null)
        } else {
          val runtime = new RuntimePickler(classLoader, clazz, tag)
          runtime.mkPickler
        }
        GlobalRegistry.picklerMap += (className -> (x => pickler))
        pickler

      case Some(existingPickler) =>
        existingPickler(tag)
    }
  }
}

// marker trait to indicate a generated pickler
// this is important for the dispatch between custom and generated picklers
trait Generated

object SPickler extends CorePicklersUnpicklers with RuntimePicklersUnpicklers

@implicitNotFound(msg = "Cannot generate an unpickler for ${T}. Recompile with -Xlog-implicits for details")
trait Unpickler[T] {
  def unpickle(tag: => FastTypeTag[_], reader: PReader): Any
  def tag: FastTypeTag[T]
}

trait GenOpenSumUnpicklers {
  implicit def genOpenSumUnpickler[T]: Unpickler[T] with Generated = macro Compat.OpenSumUnpicklerMacro_impl[T]
}

trait GenUnpicklers extends GenOpenSumUnpicklers with RuntimePicklersUnpicklers {

  implicit def genUnpickler[T]: Unpickler[T] with Generated = macro Compat.UnpicklerMacros_impl[T]

  // Note: parameter `tag` may be `null`.
  def genUnpickler(mirror: Mirror, tag: FastTypeTag[_])(implicit share: refs.Share): Unpickler[_] = {
    // println(s"generating runtime unpickler for ${tag.key}") // NOTE: needs to be an explicit println, so that we don't occasionally fallback to runtime in static cases
    val className = tag.key
    GlobalRegistry.unpicklerMap.get(className) match {
      case None =>
        // debug(s"!!! could not find registered unpickler for class $className !!!")
        val unpickler = if (className.startsWith("scala.Array")) {
          // debug(s"runtime unpickling of an array: $className")
          val elemTypeString = className.substring(12, className.length - 1)
          // debug(s"creating tag for element type: $elemTypeString")
          val elemTag = FastTypeTag(mirror, elemTypeString)
          val elemClass = Classes.classFromString(elemTypeString)
          val elemUnpickler = Unpickler.genUnpickler(mirror, elemTag)

          mkRuntimeTravPickler[Array[AnyRef]](elemClass, elemTag, tag, null, elemUnpickler)
        } else {
          val runtime = if (share.isInstanceOf[refs.ShareNothing]) {
              // debug(s"@@@ creating ShareNothingInterpretedUnpicklerRuntime for type $className")
              new ShareNothingInterpretedUnpicklerRuntime(mirror, tag)
            } else {
              // debug(s"@@@ creating InterpretedUnpicklerRuntime for type $className")
              new InterpretedUnpicklerRuntime(mirror, tag)
            }
          runtime.genUnpickler
        }
        GlobalRegistry.unpicklerMap += (className -> unpickler)
        unpickler
      case Some(existingUnpickler) =>
        existingUnpickler
    }
  }
}

object Unpickler extends CorePicklersUnpicklers with RuntimePicklersUnpicklers
