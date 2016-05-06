package scala.pickling

import scala.pickling.binary.BinaryFormats
import scala.pickling.internal.HybridRuntime
import scala.pickling.json.JsonFormats
import scala.pickling.pickler.AllPicklers

trait PicklingProtocol extends {
  val oldRuntime = internal.currentRuntime
  val onlyLookup = {
    val currentRuntime =
      if(oldRuntime.isInstanceOf[HybridRuntime]) oldRuntime
      else new HybridRuntime
    internal.replaceRuntime(currentRuntime)
  }
} with Ops with AllPicklers

object JsonPicklingProtocol extends PicklingProtocol with JsonFormats

object BinaryPicklingProtocol extends PicklingProtocol with BinaryFormats
