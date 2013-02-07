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

  implicit val pickleFormat = new JSONPickleFormat

  class JSONPickleFormat extends PickleFormat {

    def genTypeTemplate(c: Context)(tpe: c.universe.Type): Any = tpe.toString

    def genObjectTemplate(ir: ObjectIR): Chunked = {
      // each element in this list is a pair (List[Any], List[FieldIR]) for each field
      // example for one element: (List("name: ", "\n"), List(FieldIR("name")))
      val fieldTemplates: List[Chunked] = ir.fields.map(genFieldTemplate _).map {
        case (List(beginning, end), holes) => (List(",\n" + beginning, end), holes)
      }

      val initialFieldChunks: List[Any] = fieldTemplates.map(_._1).flatten
      val fieldHoles: List[FieldIR]     = fieldTemplates.map(_._2).flatten

      val withoutFirstAndLast = initialFieldChunks.tail.init

      val pairs = withoutFirstAndLast.init zip withoutFirstAndLast.tail
      val fieldChunks =
        initialFieldChunks.head +:
        (pairs map { case (left, right) => concatChunked(left, right) }) :+
        initialFieldChunks.last

      val objectHeaderChunk: String = "{\n  \"tpe\": \"" + ir.tpe + "\""

      val objectFooterChunk: String = "\n}"

      val firstChunk = concatChunked(objectHeaderChunk, fieldChunks.head)
      val lastChunk  = concatChunked(fieldChunks.last, objectFooterChunk)

      // need to group all chunks and all holes together in separate lists
      val allChunks = List(firstChunk) ++ fieldChunks.tail.init ++ List(lastChunk)
      val allHoles  = fieldHoles
      (allChunks, allHoles)
    }

    def genFieldTemplate(ir: FieldIR): Chunked = (List("  \"" + ir.name + "\": \"", "\""), List(ir))

    def concatChunked(c1: Any, c2: Any): Any =
      c1.asInstanceOf[String] + c2.asInstanceOf[String]
  }
}


// "{ \"tpe\": \"" + pickleType(c)(tpe) + "\"\n" +
//           genFields(fields).splice(vals) + "\n" +
//           "}"