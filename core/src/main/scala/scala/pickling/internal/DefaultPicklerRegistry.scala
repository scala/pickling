package scala.pickling
package internal

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.reflect.runtime.universe.Mirror
import scala.pickling.spi.{PicklerRegistry, RuntimePicklerGenerator}



/** Default pickle registry just uses TrieMaps and delgates behavior to a runtime pickler generator. */
final class DefaultPicklerRegistry(generator: RuntimePicklerGenerator) extends PicklerRegistry with RuntimePicklerRegistryHelper {
  // TODO - We need to move the special encoding for runtime classes into here, rather than in magical traits.

  private val picklerMap: mutable.Map[String, Pickler[_]] = new TrieMap[String, Pickler[_]]
  private val unpicklerMap: mutable.Map[String, Unpickler[_]] = new TrieMap[String, Unpickler[_]]

  // During constrcution, we can now register the default picklers against our cache of picklers.
  autoRegisterDefaults()

  /** Looks up the registered unpickler using the provided tagKey. */
  override def genUnpickler(mirror: Mirror, tagKey: String)(implicit share: refs.Share): Unpickler[_] = {
    unpicklerMap.get(tagKey) match {
      case Some(p) => p
      case None =>
        val p = generator.genUnpickler(mirror, tagKey)
        registerUnpickler(tagKey, p)
        p
    }
  }
  def genPickler(classLoader: ClassLoader, clazz: Class[_], tag: FastTypeTag[_])(implicit share: refs.Share): Pickler[_] = {
    val className = if (clazz == null) "null" else clazz.getName
    picklerMap.get(className) match {
      case Some(p) => p
      case None =>
        val p = generator.genPickler(classLoader, clazz, tag)
        registerPickler(className, p)
        p
    }
  }

  /** Registers a pickler with this registry for future use. */
  override def registerPickler(key: String, p: Pickler[_]): Unit =
    picklerMap.put(key, p)

  /** Registers an unpickler with this registry for future use. */
  override def registerUnpickler(key: String, p: Unpickler[_]): Unit =
    unpicklerMap.put(key, p)

  /** Checks the existince of an unpickler. */
  override def lookupUnpickler(key: String): Option[Unpickler[_]] = unpicklerMap.get(key)
}
