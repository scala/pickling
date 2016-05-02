package scala.pickling.registry

import org.scalatest.FunSuite

import scala.pickling._
import scala.pickling.internal.HybridRuntime
import scala.pickling.json.JsonFormats
import scala.pickling.pickler.AllPicklers

object Protocol extends {
  val oldRuntime = internal.currentRuntime
  val currentRuntime = new HybridRuntime
  val onlyLookup = internal.replaceRuntime(currentRuntime)
} with Ops with AllPicklers with JsonFormats

class SwitchRuntimeRegistryInit extends FunSuite {

  import Protocol._

  test("registry should be initialized when switching runtime strategies") {

    case class Foo(i: Int)
    val pf = implicitly[AbstractPicklerUnpickler[Foo]]
    
    // If the test passes, this should not initialize
    // the registry again. If it fails it does.
    implicitly[Pickler[List[String]]]

    try {
    val lookup = currentRuntime.picklers.lookupPickler(pf.tag.key)
    assert(lookup !== None)

    val pf2 = implicitly[AbstractPicklerUnpickler[Foo]]
    val lookup2 = currentRuntime.picklers.lookupPickler(pf.tag.key)
    assert(lookup === lookup2)
    } finally {
      internal.replaceRuntime(oldRuntime)
      // Just in case it doesn't get init
      implicitly[Pickler[List[String]]]
    }

  }

}
