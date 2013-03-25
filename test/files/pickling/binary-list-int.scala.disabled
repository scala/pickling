/*
  Custom pickler/unpickler test.

  Custom pickler/unpickler for type `List`, currently assuming Scala binary format
  (although typically custom picklers/unpicklers don't need to be specialized on a PickleFormat)
*/

import scala.pickling._
import binary._
import reflect.runtime.universe._

package scala.pickling {
  class Custom
}

object Test extends App {

  implicit def genListPickler[T: TypeTag](implicit elemPickler: Pickler[T], pf: PickleFormat): Pickler[List[T]] with Unpickler[List[T]] = new Pickler[List[T]] with Unpickler[List[T]] {
    import reflect.runtime.{ universe => ru }
    import ru._

    val format: PickleFormat = pf

    // this is only for reading the type during unpickling. otherwise all happens at compile-time
    val rtm = ru.runtimeMirror(getClass.getClassLoader)

    def pickle(picklee: List[T], builder: PickleBuilder): Unit = {
      builder.beginEntryNoType(typeTag[AnyRef], picklee)

      builder.putField("numElems", b => {
        b.beginEntryNoType(typeTag[Int], picklee.length)
        b.endEntry()
      })

      for (el <- picklee) {
        builder.putField("elem", b => { // in this case, the name "elem" is actually ignored for binary format, would be terrible if `format` was JSON
          elemPickler.pickle(el, b)
        })
      }

      builder.endEntry()
    }

    def unpickle(tag: TypeTag[_], reader: PickleReader): Any = {
      val r2 = reader.readField("numElems")
      val num = r2.readPrimitive(typeTag[Int]).asInstanceOf[Int]
      println(s"original list contained $num elements")

      var currReader: PickleReader = null
      var list = List[T]()
      for (i <- 1 to num) {
        currReader = reader.readField("elem")
        val el = currReader.readPrimitive(typeTag[T]).asInstanceOf[T]
        list = list :+ el
      }

      list
    }
  }

  val intPickler     = implicitly[Pickler[Int]]
  val pf             = implicitly[BinaryPickleFormat]
  val listPickler    = implicitly[Pickler[List[Int]]]
  val listUnpickler  = listPickler

  val l = List[Int](7, 24, 30)

  val builder = pf.createBuilder()

  listPickler.pickle(l, builder)
  val pckl = builder.result()
  println(pckl.value.asInstanceOf[Array[Byte]].mkString("[", ",", "]"))

  val res = listUnpickler.unpickle(typeTag[Int], pf.createReader(pckl))
  println(res)
}
