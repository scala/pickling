package scala.pickling

import scala.collection.mutable
import scala.collection.concurrent.TrieMap

object GlobalRegistry {
  val picklerMap: mutable.Map[String, SPickler[_]] =
    new TrieMap[String, SPickler[_]]
  val unpicklerMap: mutable.Map[String, Unpickler[_]] =
    new TrieMap[String, Unpickler[_]]
}
