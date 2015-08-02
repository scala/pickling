package scala.pickling
package runtime

import scala.collection.mutable
import scala.collection.concurrent.TrieMap

object GlobalRegistry {
  val picklerMap: mutable.Map[String, FastTypeTag[_] => Pickler[_]] =
    new TrieMap[String, FastTypeTag[_] => Pickler[_]]

  // Unpickling now always uses the registry.
  object unpicklerMap {
    def +=(kv: (String, Unpickler[_])): Unit = {
      internal.currentRuntime.picklers.registerUnpickler(kv._1, kv._2)
    }
    def get(key: String): Option[Unpickler[_]] = internal.currentRuntime.picklers.lookupUnpickler(key)
  }
}
