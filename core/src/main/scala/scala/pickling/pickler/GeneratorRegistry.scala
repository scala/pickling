package scala.pickling.pickler

import scala.pickling.PicklingErrors.{FailedUnpicklerGeneration, FailedPicklerGeneration}
import scala.pickling.spi.PicklerRegistry
import scala.pickling.tags.FastTypeTag

/** Register some generators at boot time so that runtime generation can
  * reuse them and not try to reflectively know what to do to generate
  * the picklers and unpicklers.
  *
  * This trait has no relation with the generation of pickler/unpicklers,
  * although they share the same denotation.
  *
  * NOTE: We may want to change these names in future releases for the sake
  * of clarity.
  */
trait GeneratorRegistry {

  import PicklerRegistry._
  import scala.pickling.internal.currentRuntime

  def registerGen[T](tag: String, gen: PicklerUnpicklerGen[T]) =
    currentRuntime.picklers.registerPicklerUnpicklerGenerator(tag, gen)

}

trait GeneratorHelper {

  import scala.language.higherKinds
  import scala.pickling.{Pickler, Unpickler}
  import scala.pickling.internal.currentRuntime

  type FastTypeTagSpecializer[T] = FastTypeTag[_] => FastTypeTag[T]

  /** Creates a pickling generator that can be registered at runtime. */
  def generateHelper[T](elementTagExtractor: FastTypeTagSpecializer[T])
                       (tpe: FastTypeTag[_]): (Pickler[T], Unpickler[T]) = {

    val elementType = elementTagExtractor(tpe)
    val elementKey = elementType.key

    def get[PU[_], S](from: String => Option[PU[_]], error: => Throwable): PU[S] = {
      if(elementKey == FastTypeTag.Any.key)
        AnyPicklerUnpickler.asInstanceOf[PU[S]]
      else from(elementKey).getOrElse(throw error).asInstanceOf[PU[S]]
    }

    val elemPickler = get[Pickler, T](currentRuntime.picklers.lookupPickler,
      new FailedPicklerGeneration(tpe.toString, elementType.toString))
    val elemUnpickler = get[Unpickler, T](currentRuntime.picklers.lookupUnpickler,
      new FailedUnpicklerGeneration(tpe.toString, elementType.toString))

    (elemPickler, elemUnpickler)
  }

  /** Specialize a [[FastTypeTag]] for a type [[T]]. */
  def specialize[T](tag: FastTypeTag[_]): FastTypeTag[T] =
    tag.asInstanceOf[FastTypeTag[T]]

}
