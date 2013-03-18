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

    // these type aliases seem like unnecessary boilerplate that we might be able to get rid of
    type PickleFormatType = PickleFormat
    type PickleBuilderType = elemPickler.PickleBuilderType
    type PickleReaderType = PickleReader

    val format: PickleFormat = pf

    // this is only for reading the type during unpickling. otherwise all happens at compile-time
    val rtm = ru.runtimeMirror(getClass.getClassLoader)

    def pickle(picklee: Any, builder: PickleBuilderType): Unit = {
      val list = picklee.asInstanceOf[List[T]]

      builder.beginEntry(typeOf[AnyRef], picklee)

      builder.putField("numElems", b => {
        b.beginEntry(typeOf[Int], list.length)
        b.endEntry()
      })

      for (el <- list) {
        builder.putField("elem", b => { // in this case, the name "elem" is actually ignored for binary format, would be terrible if `format` was JSON
          elemPickler.pickle(el, b)
        })
      }

      builder.endEntry()
    }

    def unpickle(tpe: Type, reader: PickleReaderType): Any = {
      val tpe = reader.readType(rtm) // should be "Custom"
      val r2 = reader.readField("numElems")
      val itpe = r2.readType(rtm)
      val num = r2.readPrimitive(typeOf[Int]).asInstanceOf[Int]
      println(s"original list contained $num elements")

      var currReader: PickleReader = null
      var list = List[T]()
      for (i <- 1 to num) {
        currReader = reader.readField("elem")
        val itpe = currReader.readType(rtm)
        val el = currReader.readPrimitive(typeOf[T]).asInstanceOf[T]
        list = list :+ el
      }

      list
    }
  }

  val intPickler     = implicitly[Pickler[Int]]
  val pf             = implicitly[BinaryPickleFormat]
  val listPicklerRaw = implicitly[Pickler[List[Int]]]

  val l = List[Int](7, 24, 30)

  val builder = pf.createBuilder()
  val listPickler = listPicklerRaw.asInstanceOf[Pickler[_]{ type PickleBuilderType = pf.PickleBuilderType }]

  listPickler.pickle(l, builder)
  val pckl = builder.result()
  println(pckl.value.asInstanceOf[Array[Byte]].mkString("[", ",", "]"))

  val listUnpickler = listPicklerRaw.asInstanceOf[Unpickler[_]{ type PickleBuilderType = BinaryPickleBuilder ; type PickleReaderType = BinaryPickleReader }]

  val res = listUnpickler.unpickle(typeOf[Int], pf.createReader(pckl))
  println(res)
}
