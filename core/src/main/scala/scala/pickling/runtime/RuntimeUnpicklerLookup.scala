package scala.pickling
package runtime

import scala.reflect.runtime.universe.Mirror
import internal.Classes


object RuntimeUnpicklerLookup extends RuntimePicklersUnpicklers {
  // TODO - Deprecate and remove!
  def genUnpickler(mirror: Mirror, tagKey: String)(implicit share: refs.Share): Unpickler[_] =
    internal.currentRuntime.picklers.genUnpickler(mirror, tagKey)
}
