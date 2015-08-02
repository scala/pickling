package scala.pickling
package runtime

import scala.reflect.runtime.{universe => ru}

object RuntimePicklerLookup extends RuntimePicklersUnpicklers {
  def genPickler(classLoader: ClassLoader, clazz: Class[_], tag: FastTypeTag[_])(implicit share: refs.Share): Pickler[_] =
    internal.currentRuntime.picklers.genPickler(classLoader, clazz, tag)
}
