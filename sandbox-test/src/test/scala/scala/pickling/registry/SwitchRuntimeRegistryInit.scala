package scala.pickling.registry

import org.scalatest.FunSuite

import scala.pickling._
import scala.pickling.internal.HybridRuntime
import scala.pickling.json.JsonFormats
import scala.pickling.pickler.AllPicklers

class SwitchRuntimeRegistryInit extends FunSuite {

  import JsonPicklingProtocol._

  test("registry should be initialized when switching runtime strategies") {

    import scala.pickling.internal.currentRuntime

    case class Foo(i: Int)
    val pf = implicitly[AbstractPicklerUnpickler[Foo]]
    
    // If the test passes, this should not initialize
    // the registry again. If it fails it does.
    implicitly[Pickler[List[String]]]

    val lookup = currentRuntime.picklers.lookupPickler(pf.tag.key)
    assert(lookup !== None)

    val pf2 = implicitly[AbstractPicklerUnpickler[Foo]]
    val lookup2 = currentRuntime.picklers.lookupPickler(pf.tag.key)
    assert(lookup === lookup2)

  }

}
