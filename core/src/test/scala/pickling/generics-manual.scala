package scala.pickling.generics.manual

import org.scalatest.FunSuite
import scala.pickling._
import binary._
import reflect.runtime.universe._

class Base
case class Generic[T](x: T) extends Base {
  override def toString =
    s"Generic(${ x.toString })"
}
case class NonGeneric(s: String) extends Base
case class Person(age: Int)

class GenericsManualTest extends FunSuite {
  test("main") {
    // these methods are used to simulate the dispatch step
    def pickleManual[T](picklee: T)(implicit pf: PickleFormat): Pickle = {
      picklee.getClass match {
        case clazz if clazz == classOf[Generic[_]] =>
          val picklerRuntime = new InterpretedPicklerRuntime(getClass.getClassLoader, clazz)
          val pickler = picklerRuntime.genPickler.asInstanceOf[SPickler[T]]
          val builder = pf.createBuilder()
          pickler.pickle(picklee, builder)
          builder.result()
        case clazz if clazz == classOf[NonGeneric] => ???
      }
    }

    def unpickleManual[T: TypeTag](pickle: Pickle)(implicit pf: PickleFormat): T = {
      val mirror = runtimeMirror(getClass.getClassLoader)

      val reader = pf.createReader(pickle.asInstanceOf[pf.PickleType], mirror)
      // read tag: "Generic"
      reader.hintTag(implicitly[FastTypeTag[T]])
      reader.pinHints()
      val tag = reader.beginEntry()
      debug("unpickleManual: tag = " + tag)
      debug("creating unpickler for that tag...")

      val unpicklerRuntime = new InterpretedUnpicklerRuntime(mirror, tag)
      val unpickler = unpicklerRuntime.genUnpickler

      unpickler.unpickle(tag, reader).asInstanceOf[T]
    }

    val g: Base = Generic(Person(42))
    val p = pickleManual[Base](g)
    assert(p.value.asInstanceOf[Array[Byte]].mkString("[", ",", "]") === "[0,0,0,38,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,103,101,110,101,114,105,99,115,46,109,97,110,117,97,108,46,71,101,110,101,114,105,99,0,0,0,37,115,99,97,108,97,46,112,105,99,107,108,105,110,103,46,103,101,110,101,114,105,99,115,46,109,97,110,117,97,108,46,80,101,114,115,111,110,0,0,0,42]")

    val ug: Base = unpickleManual[Base](p)
    assert(ug === g)
  }
}
