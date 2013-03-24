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

object Test extends App {

  // these methods are used to simulate the dispatch step
  def pickleManual[T](picklee: T)(implicit pf: PickleFormat): Pickle = {
    picklee.getClass match {
      case clazz if clazz == classOf[Generic[_]] =>
        val picklerRuntime = new InterpretedPicklerRuntime(getClass.getClassLoader, clazz)
        val pickler = picklerRuntime.genPickler.asInstanceOf[Pickler[T]]
        val builder = pf.createBuilder()
        pickler.pickle(picklee, builder)
        builder.result()
      case clazz if clazz == classOf[NonGeneric] => ???
    }
  }

  def unpickleManual[T](pickle: Pickle)(implicit pf: PickleFormat): T = {
    val mirror = runtimeMirror(getClass.getClassLoader)

    val reader = pf.createReader(pickle.asInstanceOf[pf.PickleType])
    // read tag: "Generic"
    val tag = reader.readTag(mirror)
    debug("unpickleManual: tag = " + tag)
    debug("creating unpickler for that tag...")

    val unpicklerRuntime = new InterpretedUnpicklerRuntime(mirror, tag)
    val unpickler = unpicklerRuntime.genUnpickler

    unpickler.unpickle(tag, reader).asInstanceOf[T]
  }

  val g: Base = Generic(Person(42))
  val p = pickleManual[Base](g)
  println(p.value.asInstanceOf[Array[Byte]].mkString("[", ",", "]"))

  val ug: Base = unpickleManual[Base](p)
  println("unpickled: " + ug)
}
