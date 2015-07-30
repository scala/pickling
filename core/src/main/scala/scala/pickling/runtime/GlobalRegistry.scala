package scala.pickling
package runtime

import scala.collection.mutable
import scala.collection.concurrent.TrieMap

object GlobalRegistry {
  val picklerMap: mutable.Map[String, FastTypeTag[_] => Pickler[_]] =
    new TrieMap[String, FastTypeTag[_] => Pickler[_]]

  val unpicklerMap: mutable.Map[String, Unpickler[_]] =
    new TrieMap[String, Unpickler[_]]
}
