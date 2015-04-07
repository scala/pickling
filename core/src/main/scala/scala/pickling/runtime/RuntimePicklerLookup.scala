package scala.pickling
package runtime

import scala.reflect.runtime.{universe => ru}


object RuntimePicklerLookup extends RuntimePicklersUnpicklers {
  // TODO - We should lock the GRL before running this method, in case we generate any picklers.
  def genPickler(classLoader: ClassLoader, clazz: Class[_], tag: FastTypeTag[_])(implicit share: refs.Share): Pickler[_] = {
    // println(s"generating runtime pickler for $clazz") // NOTE: needs to be an explicit println, so that we don't occasionally fallback to runtime in static cases
    val className = if (clazz == null) "null" else clazz.getName
    GlobalRegistry.picklerMap.get(className) match {
      case None =>
        // debug(s"!!! could not find registered pickler for class $className, tag ${tag.key} !!!")
        val pickler: Pickler[_] = if (clazz.isArray) {
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
