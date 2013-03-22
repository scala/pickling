import scala.pickling._
import binaryopt._
import reflect.runtime.universe._

case class Person(name: String, age: Int)

object ListPersonBench extends testing.Benchmark {

  implicit def genListPickler[T: TypeTag](implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T], pf: PickleFormat): Pickler[List[T]] with Unpickler[List[T]] = new Pickler[List[T]] with Unpickler[List[T]] {
    import scala.collection.mutable.ListBuffer
    import scala.reflect.runtime.currentMirror

    val format: PickleFormat = pf

    def pickle(picklee: Any, builder: PickleBuilder): Unit = {
      val list = picklee.asInstanceOf[List[T]]
      val tpe  = typeTag[Int]
      builder.beginEntryNoType(typeTag[AnyRef], picklee)

      builder.putField("numElems", b => {
        b.beginEntryNoType(tpe, list.length)
        b.endEntry()
      })

      for (el <- list) {
        builder.putField("elem", b => { // in this case, the name "elem" is actually ignored for binary format, would be terrible if `format` was JSON
          elemPickler.pickle(el, b)
        })
      }

      builder.endEntry()
    }

    def unpickle(tpe: TypeTag[_], reader: PickleReader): Any = {
      //val tpe  = typeTag[T]
      val next = reader.readField("numElems")
      val num  = next.readPrimitive(typeTag[Int]).asInstanceOf[Int]

      var listbuf = ListBuffer[T]()
      for (i <- 1 to num) {
        reader.readField("elem")
        //val el = currReader.readPrimitive(tpe).asInstanceOf[T] //TODO: would like to use currReader.unpickle[T] here
        //val el = reader.unpickle[T]

        val tag = reader.readTag(currentMirror)
        val el  = elemUnpickler.unpickle(tag, reader)

        listbuf += el.asInstanceOf[T]
      }

      listbuf.toList
    }
  }

  val name = "Jim"
  val lst = (1 to 100000).map(i => Person(name+i, i)).toList

  val pf             = implicitly[BinaryPickleFormat]
  val listPicklerRaw = implicitly[Pickler[List[Person]]]

  override def run() {
    val builder = pf.createBuilder()
    val listPickler = listPicklerRaw.asInstanceOf[Pickler[_]]

    listPickler.pickle(lst, builder)
    val pckl = builder.result()

    val listUnpickler = listPicklerRaw.asInstanceOf[Unpickler[_]]

    val res = listUnpickler.unpickle(typeTag[List[Person]], pf.createReader(pckl))
  }
}

// object PicklerUnsafeListBench extends testing.Benchmark {
//   import UnsafePickler._
//   val lst = (1 to 100000).toList
//   val arr: Array[Byte] = Array.fill[Byte](lst.length * 4 + 4)(0)

//   override def run() {
//     UnsafePickler.unsafeListPickler[Int].pickle(arr, 0, lst)
//     // println("Size: "+arr.length)
//     val res = UnsafePickler.unsafeListPickler[Int].unpickle(arr, 0)
//   }
// }

// object PicklerSeqListBench extends testing.Benchmark {
//   import Pickler._
//   val lst = (1 to 100000).toList
//   val arr: Array[Byte] = Array.fill[Byte](lst.length * 4 + 4)(0)

//   override def run() {
//     seqPickler[Int].pickle(arr, 0, lst)
//     val res = seqPickler[Int].unpickle(arr, 0)
//   }
// }
