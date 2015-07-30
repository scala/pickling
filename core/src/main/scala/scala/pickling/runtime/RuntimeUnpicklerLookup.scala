package scala.pickling
package runtime

import scala.reflect.runtime.universe.Mirror
import internal.Classes


object RuntimeUnpicklerLookup extends RuntimePicklersUnpicklers {
  // Note: parameter `tag` may be `null`.
  // TODO - This method is one which would need the GRL, if we try to avoid using it in every
  //        unpickle scenario.
  def genUnpickler(mirror: Mirror, tagKey: String)(implicit share: refs.Share): Unpickler[_] = {
    // println(s"generating runtime unpickler for ${tagKey}") // NOTE: needs to be an explicit println, so that we don't occasionally fallback to runtime in static cases
    GlobalRegistry.unpicklerMap.get(tagKey) match {
      case None =>
        // debug(s"!!! could not find registered unpickler for class $tagKey !!!")
        val unpickler = if (tagKey.startsWith("scala.Array")) {
          // debug(s"runtime unpickling of an array: $tagKey")
          val elemTypeString = tagKey.substring(12, tagKey.length - 1)
          // debug(s"creating tag for element type: $elemTypeString")
          val elemTag = FastTypeTag(mirror, elemTypeString)
          val elemClass = Classes.classFromString(elemTypeString)
          val elemUnpickler = genUnpickler(mirror, elemTypeString)
          val tag = FastTypeTag(mirror, tagKey)

          mkRuntimeTravPickler[Array[AnyRef]](elemClass, elemTag, tag, null, elemUnpickler)
        } else {
          internal.GRL.lock()
          try {
            val runtime = if (share.isInstanceOf[refs.ShareNothing]) {
              // debug(s"@@@ creating ShareNothingInterpretedUnpicklerRuntime for type $tagKey")
              new ShareNothingInterpretedUnpicklerRuntime(mirror, tagKey)
            } else {
              // debug(s"@@@ creating InterpretedUnpicklerRuntime for type $tagKey")
              new InterpretedUnpicklerRuntime(mirror, tagKey)
            }
            runtime.genUnpickler
          } finally internal.GRL.unlock()
        }
        GlobalRegistry.unpicklerMap += (tagKey -> unpickler)
        unpickler
      case Some(existingUnpickler) =>
        existingUnpickler
    }
  }
}
