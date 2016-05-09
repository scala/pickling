package scala.pickling.pickler

import scala.pickling.PicklingErrors.{TypeMismatch, FailedPicklerGeneration}
import scala.pickling.{Pickler, Unpickler}
import scala.pickling.spi.PicklerRegistry
import scala.pickling.tags.FastTypeTag

/** Register some generators at boot time so that runtime generation can
  * reuse them and not try to reflectively figure out the structure of
  * classes to create picklers and unpicklers accordingly.
  *
  * This trait has no relation with the generation of pickler/unpicklers,
  * although they share the same denotation.
  */
trait GeneratorRegistry {

  import PicklerRegistry._
  import scala.pickling.internal.currentRuntime

  def registerPicklerAsGen[T](pu: Pickler[T] with Unpickler[T]) =
    currentRuntime.picklers.registerPicklerUnpicklerGenerator(
      pu.tag.typeConstructor, _ => pu
    )

  def registerGen[T](tag: String, gen: PicklerUnpicklerGen[T]) =
    currentRuntime.picklers.registerPicklerUnpicklerGenerator(tag, gen)

}

/** Mix-in and make use of generation-related code that runtime
  * picklers usually need for unknown type parameters as [[Any]].
  */
trait GeneratorHelper {

  import scala.language.higherKinds
  import scala.pickling.{Pickler, Unpickler}
  import scala.pickling.internal.currentRuntime

  type FastTypeTagSpecializer[T] = FastTypeTag[_] => FastTypeTag[T]

  type GenPicklerSignature[PU[_]] =
    (ClassLoader, Class[_], FastTypeTag[_]) => PU[_]

  /** Generic get that abstracts over [[Pickler]] and [[Unpickler]]. */
  def get[PU[_], S](query: String => Option[PU[_]],
                    key: String, error: => Throwable): PU[S] = {
    if(key == FastTypeTag.Any.key)
      AnyPicklerUnpickler.asInstanceOf[PU[S]]
    else query(key).getOrElse(throw error).asInstanceOf[PU[S]]
  }

  /** Get a pickler from the registry or throw exception otherwise. */
  def getPickler[T, S](tpe: FastTypeTag[T], fullTpe: FastTypeTag[S]) =
    get[Pickler, T](currentRuntime.picklers.lookupPickler, tpe.key,
      new FailedPicklerGeneration(fullTpe.toString, tpe.toString))

  /** Get a unpickler from the registry or throw exception otherwise. */
  def getUnpickler[T, S](tpe: FastTypeTag[T], fullTpe: FastTypeTag[S]) =
    get[Unpickler, T](currentRuntime.picklers.lookupUnpickler, tpe.key,
      new FailedPicklerGeneration(fullTpe.toString, tpe.toString))

  /** Creates a pickling generator that can be registered at runtime. */
  def generateHelper[T](elementTagExtractor: FastTypeTagSpecializer[T])
                       (tpe: FastTypeTag[_]): (Pickler[T], Unpickler[T]) = {

    val elementType = elementTagExtractor(tpe)
    getPickler(elementType, tpe) -> getUnpickler(elementType, tpe)

  }

  /** Specialize a [[FastTypeTag]] for a type [[T]]. */
  def specialize[T](tag: FastTypeTag[_]): FastTypeTag[T] =
    tag.asInstanceOf[FastTypeTag[T]]

  /** Extract one type parameter from a type constructor and
    * cast them to a concrete type [[T]].
    *
    * @tparam T Type we want to convert to
    *
    * @return A tag holding information about [[T]]
    */
  def oneArgumentTagExtractor[T](tpe: FastTypeTag[_]): FastTypeTag[T] = {
    val typeArgs = tpe.typeArgs
    typeArgs match {
      case List(one) => one.asInstanceOf[FastTypeTag[T]]
      // TODO: Remove this hack to handle `Nil.type` as a tag
      case List() => FastTypeTag.Any.asInstanceOf[FastTypeTag[T]]
      case x => throw TypeMismatch(List(tpe), typeArgs)
    }
  }

  /** Extract two type parameters from a type constructor and
    * cast them to some concrete types [[T]] and [[S]].
    *
    * @tparam T First type we want to convert to
    * @tparam S Second type we want to convert to
    *
    * @return A tuple of tags of ([[T]], [[S]])
    */
  def twoArgumentTagExtractor[T, S](tpe: FastTypeTag[_]): (FastTypeTag[T], FastTypeTag[S]) = {
    val typeArgs = tpe.typeArgs
    typeArgs match {
      case List(one, two) =>
        one.asInstanceOf[FastTypeTag[T]] -> two.asInstanceOf[FastTypeTag[S]]
      case _ => throw TypeMismatch(List(tpe), typeArgs)
    }
  }

}
