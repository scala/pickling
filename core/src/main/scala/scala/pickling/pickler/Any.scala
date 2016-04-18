package scala.pickling
package pickler

import scala.pickling.internal.GRL
import scala.pickling.internal.currentRuntime
import scala.reflect.runtime.currentMirror

/** Generate a [[Pickler]] and [[Unpickler]] for [[Any]].
  *
  * It will look up pickler/unpickler at runtime, or generate it.
  */
object AnyPicklerUnpickler extends AbstractPicklerUnpickler[Any]
    with AutoRegister[Any] {

  override def tag: FastTypeTag[Any] = FastTypeTag.Any

  /** Pickle [[Any]] by getting its class at runtime and looking
    * up the correct [[Pickler]] for that class if available or
    * generate it.
    *
    * Don't use [[AnyPicklerUnpickler]] for pickling classes with generic
    * types. Otherwise, it will fail because of the type erasure
    * and the lookup will replace the unknown type by [[Any]].
    */
  override def pickle(picklee: Any, builder: PBuilder): Unit = {
    val clazz = picklee.getClass
    val classLoader = this.getClass.getClassLoader
    GRL.lock()
    val tag = try FastTypeTag.makeRaw(clazz)
    finally GRL.unlock()
    val p = currentRuntime.picklers.genPickler(classLoader, clazz, tag)
    p.asInstanceOf[Pickler[Any]].pickle(picklee, builder)
  }

  /** Unpickle something as [[Any]] by looking up registered
    * unpicklers for [[tag]] or using runtime unpickler generation.
    */
  def unpickle(tag: String, reader: PReader): Any = {
    val actualUnpickler = currentRuntime.picklers.genUnpickler(currentMirror, tag)
    actualUnpickler.unpickle(tag, reader)
  }

  override def toString = "AnyPicklerUnpickler"
}

trait AnyUnpicklers {
  implicit val anyPicklerUnpickler: AbstractPicklerUnpickler[Any] = AnyPicklerUnpickler
}
