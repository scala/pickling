/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.pickling

package object json {
  import reflect.macros.Context
  import ir._

  // each pickle formatter needs to provide hard-coded picklers for
  // all of the primitive types. this is because there are many ways
  // to pickle an Int, for example, we can't guess what the right way
  // is as a framework.

  private val numericPickler: Pickler[AnyVal] = new Pickler[AnyVal] { def pickle(x: Any) = new Pickle { val value: Any = x }}

  implicit val intPickler: Pickler[Int]         = numericPickler
  implicit val longPickler: Pickler[Long]       = numericPickler
  implicit val shortPickler: Pickler[Short]     = numericPickler
  implicit val doublePickler: Pickler[Double]   = numericPickler
  implicit val floatPickler: Pickler[Float]     = numericPickler
  implicit val booleanPickler: Pickler[Boolean] = numericPickler
  implicit val bytePickler: Pickler[Byte]       = numericPickler
  implicit val charPickler: Pickler[Char]       = new Pickler[Char]  { def pickle(x: Any) = new Pickle { val value: Any = "\"" + x.toString + "\""}}
  implicit val stringPickler: Pickler[String]   = new Pickler[String]{ def pickle(x: Any) = new Pickle { val value: Any = "\"" + x.toString + "\""}}

  implicit val pickleFormat = new JSONPickleFormat

  class JSONPickleFormat extends PickleFormat {

    def genTypeTemplate(c: Context)(tpe: c.universe.Type): Any = tpe.toString

    //def genValueTemplate(c: Context)(tpe: c.universe.Type): Option[Any => Any] =

    def pairUp[T](l: List[T]): List[(T, T)] = l match {
      case fst :: snd :: rest => (fst, snd) :: pairUp(rest)
      case List() => List()
    }

    def genObjectTemplate(ir: ObjectIR): Chunked = {
      println("genObjectTemplate on "+ ir.tpe)
      println("fields: " + ir.fields)

      // each element in this list is a pair (List[Any], List[FieldIR]) for each field
      // example for one element: (List("name: ", "\n"), List(FieldIR("name")))
      val fieldTemplates: List[Chunked] = ir.fields.map(genFieldTemplate _).map {
        case (List(beginning, end), holes) => (List(",\n" + beginning, end), holes)
      }

      val initialFieldChunks: List[Any] = fieldTemplates.map(_._1).flatten
      println("initial field chunks: " + initialFieldChunks.mkString("]["))

      val fieldHoles: List[FieldIR]     = fieldTemplates.map(_._2).flatten

      val withoutFirstAndLast = initialFieldChunks.tail.init
      println("withoutFirstAndLast: " + withoutFirstAndLast.mkString("]["))

      val pairs = pairUp(withoutFirstAndLast)
      println("to be merged: " + pairs.mkString("]["))

      val fieldChunks =
        initialFieldChunks.head +:
        (pairs map { case (left, right) => concatChunked(left, right) }) :+
        initialFieldChunks.last

      println("field chunks: " + fieldChunks.mkString("]["))

      val objectHeaderChunk: String = "{\n  \"tpe\": \"" + ir.tpe + "\""

      val objectFooterChunk: String = "\n}"

      val firstChunk = concatChunked(objectHeaderChunk, fieldChunks.head)
      val lastChunk  = concatChunked(fieldChunks.last, objectFooterChunk)

      // need to group all chunks and all holes together in separate lists
      val allChunks = List(firstChunk) ++ fieldChunks.tail.init ++ List(lastChunk)
      println("all chunks: " + allChunks.mkString("]["))
      val allHoles  = fieldHoles
      (allChunks, allHoles)
    }

    def genFieldTemplate(ir: FieldIR): Chunked = (List("  \"" + ir.name + "\": ", ""), List(ir))

    def concatChunked(c1: Any, c2: Any): Any =
      c1.toString + c2.toString
  }
}


// "{ \"tpe\": \"" + pickleType(c)(tpe) + "\"\n" +
//           genFields(fields).splice(vals) + "\n" +
//           "}"