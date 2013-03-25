import scala.pickling._
import binary._
import scala.reflect.runtime.universe._
// rtm is only for reading the type during unpickling. otherwise all happens at compile-time
import scala.reflect.runtime.{ universe => ru, currentMirror => rtm }
import scala.collection.generic._

object Test extends App {

  implicit def traversablePickler[T: TypeTag, Coll[_] <: Traversable[_]]
    (implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
              pf: PickleFormat, cbf: CanBuildFrom[Coll[_], T, Coll[T]]): Pickler[Coll[T]] with Unpickler[Coll[T]] = new Pickler[Coll[T]] with Unpickler[Coll[T]] {

    val format: PickleFormat = pf

    def pickle(picklee: Any, builder: PickleBuilder): Unit = {
      val list = picklee.asInstanceOf[List[T]]

      builder.beginEntry(typeTag[AnyRef], picklee)

      builder.putField("numElems", b => {
        b.beginEntry(typeTag[Int], list.length)
        b.endEntry()
      })

      for (el <- list) {
        builder.putField("elem", b => { // in this case, the name "elem" is actually ignored for binary format, would be terrible if `format` was JSON
          elemPickler.pickle(el, b)
        })
      }

      builder.endEntry()
    }

    def unpickle(tpe: Type, reader: PickleReader): Any = {
      val ignore1 = reader.readTag(rtm)
      val r2 = reader.readField("numElems")
      val ignore2 = r2.readTag(rtm)
      val num = r2.readPrimitive(typeTag[Int]).asInstanceOf[Int]
      println(s"original list contained $num elements")

      var currReader: PickleReader = null
      var list = List[T]()
      for (i <- 1 to num) {
        currReader = reader.readField("elem")
        val ignore3 = currReader.readTag(rtm)
        val el = currReader.readPrimitive(typeTag[T]).asInstanceOf[T]
        list = list :+ el
      }

      list
    }
  }

  val l = List(7, 24, 30)
  val p = l.pickle
  println(p.value.asInstanceOf[Array[Byte]].mkString("[", ",", "]"))
  val ul = p.unpickle[List[Int]]
  println(ul)
}
