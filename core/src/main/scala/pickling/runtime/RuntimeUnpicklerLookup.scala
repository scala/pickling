package scala.pickling
package runtime

import scala.reflect.runtime.universe.Mirror
import internal.Classes


object RuntimeUnpicklerLookup extends RuntimePicklersUnpicklers {
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
          val elemUnpickler = genUnpickler(mirror, elemTag)

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
