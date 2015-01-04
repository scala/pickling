package scala.pickling

import scala.collection.mutable
import scala.collection.concurrent.TrieMap

object GlobalRegistry {
  val picklerMap: mutable.Map[String, FastTypeTag[_] => SPickler[_]] =
    new TrieMap[String, FastTypeTag[_] => SPickler[_]]

  val unpicklerMap: mutable.Map[String, Unpickler[_]] =
    new TrieMap[String, Unpickler[_]]
}
