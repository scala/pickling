import scala.pickling._
import binary._
import reflect.runtime.universe._

object PicklerTest extends App {
  // import Pickler._
  // val arr: Array[Byte] = Array.fill[Byte](1000)(0)
  // intPickler.pickle(arr, 0, 12)
  // println(intPickler.unpickle(arr, 0)._1)

  // val lst = List(5, 6, 7)
  // seqPickler[Int].pickle(arr, 4, lst)
  // val lst2 = seqPickler[Int].unpickle(arr, 4)
  // println(lst2._1)

  // val tup = (6, 7)
  // tuple2Pickler[Int, Int].pickle(arr, lst2._2, tup)
  // val tup2 = tuple2Pickler[Int, Int].unpickle(arr, lst2._2)
  // println(tup2._1)

  // val lstTup = List((1, 2), (3, 4), (5, 6))
  // seqPickler[(Int, Int)].pickle(arr, tup2._2, lstTup)
  // val lstTup2 = seqPickler[(Int, Int)].unpickle(arr, tup2._2)
  // println(lstTup2._1)

  // val lstLst = List(List(1), List(2), List(3))
  // seqPickler[Seq[Int]].pickle(arr, lstTup2._2, lstLst)
  // val lstLst2 = seqPickler[Seq[Int]].unpickle(arr, lstTup2._2)
  // println(lstLst2._1)

}

object ListBench extends testing.Benchmark {
  implicit def genListPickler[T: TypeTag](implicit elemPickler: Pickler[T], pf: PickleFormat): Pickler[List[T]] with Unpickler[List[T]] = new Pickler[List[T]] with Unpickler[List[T]] {
    import scala.collection.mutable.ListBuffer
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
      val tpe  = typeTag[Int]
      builder.beginEntryNoType(typeTag[AnyRef], picklee, 4 + 4 * list.length)

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

    def unpickle(tpe: TypeTag[_], reader: PickleReaderType): Any = {
      val tpe = typeTag[T]
      val r2 = reader.readField("numElems")
      val num = r2.readPrimitive(typeTag[Int]).asInstanceOf[Int]

      var currReader: PickleReader = null
      var listbuf = ListBuffer[T]()
      for (i <- 1 to num) {
        currReader = reader.readField("elem")
        val el = currReader.readPrimitive(tpe).asInstanceOf[T] //TODO: would like to use currReader.unpickle[T] here
        listbuf += el
      }

      listbuf.toList
    }
  }

  val lst = (1 to 10000).toList

  val intPickler     = implicitly[Pickler[Int]]
  val pf             = implicitly[BinaryPickleFormat]
  val listPicklerRaw = implicitly[Pickler[List[Int]]]

  override def run() {
    val builder = pf.createBuilder()
    val listPickler = listPicklerRaw.asInstanceOf[Pickler[_]{ type PickleBuilderType = pf.PickleBuilderType }]

    listPickler.pickle(lst, builder)
    val pckl = builder.result()
    // println(pckl.value.asInstanceOf[Array[Byte]].mkString("[", ",", "]"))

    val listUnpickler = listPicklerRaw.asInstanceOf[Unpickler[_]{ type PickleBuilderType = BinaryPickleBuilder ; type PickleReaderType = BinaryPickleReader }]

    val res = listUnpickler.unpickle(typeTag[Int], pf.createReader(pckl))
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
