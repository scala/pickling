package scala.pickling
package pickler

import scala.pickling.internal.currentRuntime
import scala.reflect.runtime.currentMirror

/** Generate a [[Pickler]] and [[Unpickler]] for [[Any]].
  *
  * It will look up pickler/unpickler at runtime, or generate it.
  */
object AnyPicklerUnpickler extends AbstractPicklerUnpickler[Any]
    with AutoRegister[Any] {

  final val nullPickler = Defaults.nullPickler.asInstanceOf[Pickler[Any]]

  override def tag: FastTypeTag[Any] = FastTypeTag.Any

  /** Pickle [[Any]] by getting its class at runtime and looking
    * up the correct [[Pickler]] for that class if available or
    * generate it.
    *
    * Don't use [[AnyPicklerUnpickler]] for pickling classes with generic
    * types. Otherwise, it will fail because of the type erasure.
    * The lookup will replace the unknown type parameters by [[Any]].
    */
  override def pickle(picklee: Any, builder: PBuilder): Unit = {

    // Use nullPickler if null, get pickler otherwise
    val pickler = if (picklee == null) nullPickler else {
      val clazz = picklee.getClass
      val classLoader = this.getClass.getClassLoader
      val tag = FastTypeTag.makeRaw(clazz)
      val p = currentRuntime.picklers.genPickler(classLoader, clazz, tag)
      p.asInstanceOf[Pickler[Any]]
    }

    pickler.pickle(picklee, builder)

  }

  /** Unpickle something as [[Any]] by looking up registered
    * unpicklers for [[tag]] or using runtime unpickler generation.
    */
  def unpickle(tag: String, reader: PReader): Any = {
    if (reader.atPrimitive) reader.readPrimitive()
    else {
      val actualUnpickler = currentRuntime.picklers.genUnpickler(currentMirror, tag)
      actualUnpickler.unpickle(tag, reader)
    }
  }

  override def toString = "AnyPicklerUnpickler"
}

trait AnyUnpicklers {
  implicit val anyPicklerUnpickler: AbstractPicklerUnpickler[Any] = AnyPicklerUnpickler
}
