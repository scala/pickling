/*
  Custom pickler/unpickler test.

  Custom pickler/unpickler for type `Person`, currently assuming Scala binary format
  (although typically custom picklers/unpicklers don't need to be specialized on a PickleFormat)
*/

import scala.pickling._
import binary._
import reflect.runtime.universe._

package scala.pickling {
  class Custom
}

object Test extends App {

  implicit def traversablePickler[T: TypeTag, Coll[_] <: Traversable[_]]
    (implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
              pf: PickleFormat, cbf: CanBuildFrom[Coll[_], T, Coll[T]]): Pickler[Coll[T]] with Unpickler[Coll[T]] = ???
  // implicit def genListPickler[T: TypeTag](implicit elemPickler: Pickler[T], pf: PickleFormat): Pickler[List[T]] with Unpickler[List[T]] = new Pickler[List[T]] with Unpickler[List[T]] {
  //   import reflect.runtime.{ universe => ru }
  //   import ru._

  //   val format: PickleFormat = pf

  //   // this is only for reading the type during unpickling. otherwise all happens at compile-time
  //   val rtm = ru.runtimeMirror(getClass.getClassLoader)

  //   def pickle(picklee: Any, builder: PickleBuilder): Unit = {
  //     val list = picklee.asInstanceOf[List[T]]

  //     builder.beginEntry(typeTag[AnyRef], picklee)

  //     builder.putField("numElems", b => {
  //       b.beginEntry(typeTag[Int], list.length)
  //       b.endEntry()
  //     })

  //     for (el <- list) {
  //       builder.putField("elem", b => { // in this case, the name "elem" is actually ignored for binary format, would be terrible if `format` was JSON
  //         elemPickler.pickle(el, b)
  //       })
  //     }

  //     builder.endEntry()
  //   }

  //   def unpickle(tpe: Type, reader: PickleReader): Any = {
  //     val tpe = reader.readTag(rtm).tpe
  //     val r2 = reader.readField("numElems")
  //     val itpe = r2.readTag(rtm).tpe
  //     val num = r2.readPrimitive(typeTag[Int]).asInstanceOf[Int]
  //     println(s"original list contained $num elements")

  //     var currReader: PickleReader = null
  //     var list = List[T]()
  //     for (i <- 1 to num) {
  //       currReader = reader.readField("elem")
  //       val itpe = currReader.readTag(rtm).tpe
  //       val el = currReader.readPrimitive(typeTag[T]).asInstanceOf[T]
  //       list = list :+ el
  //     }

  //     list
  //   }
  // }

  val l = List(7, 24, 30)
  val p = l.pickle
  println(p.value.asInstanceOf[Array[Byte]].mkString("[", ",", "]"))
  val ul = p.unpickle[List[Int]]
  println(ul)
}
