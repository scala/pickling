package scala.pickling
package internal

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.pickling.spi.{PicklerRegistry, RuntimePicklerGenerator}

/** Default pickle registry just uses TrieMaps and delgates behavior to a runtime pickler generator. */
final class DefaultPicklerRegistry(generator: RuntimePicklerGenerator) extends PicklerRegistry {
  private val picklerMap: mutable.Map[String, Pickler[_]] = new TrieMap[String, Pickler[_]]
  private val unpicklerMap: mutable.Map[String, Unpickler[_]] = new TrieMap[String, Unpickler[_]]
  /** Looks up the registered unpickler using the provided tagKey. */
  override def lookupUnpickler(tagKey: String): Unpickler[_] = {
    unpicklerMap.get(tagKey) match {
      case Some(p) => p
      case None =>
        val p = generator.genUnpickler(tagKey)
        registerUnpickler(p)
        p
    }
  }
  def genPickler(classLoader: ClassLoader, clazz: Class[_], tag: FastTypeTag[_]): Pickler[_] = {
    picklerMap.get(tag.key) match {

      case Some(p) => p
      case None =>
        val p = generator.genPickler(classLoader, clazz, tag)
        registerPickler(p)
        p
    }
  }

  /** Registers a pickler with this registry for future use. */
  override def registerPickler[T](p: Pickler[T]): Unit =
    picklerMap.put(p.tag.key, p)

  /** Registers an unpickler with this registry for future use. */
  override def registerUnpickler[T](p: Unpickler[T]): Unit =
    unpicklerMap.put(p.tag.key, p)
}
