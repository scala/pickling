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

  implicit val intPickler: Pickler[Int]         = new Pickler[Int]     { def pickle(x: Any) = new Pickle { val value: Any = x }}
  implicit val longPickler: Pickler[Long]       = new Pickler[Long]    { def pickle(x: Any) = new Pickle { val value: Any = x }}
  implicit val shortPickler: Pickler[Short]     = new Pickler[Short]   { def pickle(x: Any) = new Pickle { val value: Any = x }}
  implicit val doublePickler: Pickler[Double]   = new Pickler[Double]  { def pickle(x: Any) = new Pickle { val value: Any = x }}
  implicit val floatPickler: Pickler[Float]     = new Pickler[Float]   { def pickle(x: Any) = new Pickle { val value: Any = x }}
  implicit val booleanPickler: Pickler[Boolean] = new Pickler[Boolean] { def pickle(x: Any) = new Pickle { val value: Any = x }}
  implicit val bytePickler: Pickler[Byte]       = new Pickler[Byte]    { def pickle(x: Any) = new Pickle { val value: Any = x }}
  implicit val charPickler: Pickler[Char]       = new Pickler[Char]    { def pickle(x: Any) = new Pickle { val value: Any = "\"" + x.toString + "\""}}
  implicit val stringPickler: Pickler[String]   = new Pickler[String]  { def pickle(x: Any) = new Pickle { val value: Any = "\"" + x.toString + "\""}}

  implicit val pickleFormat = new JSONPickleFormat

  class JSONPickleFormat extends PickleFormat {

    def genTypeTemplate(c: Context)(tpe: c.universe.Type): Any = tpe.typeSymbol.name.toString

    //def genValueTemplate(c: Context)(tpe: c.universe.Type): Option[Any => Any] =

    def pairUp[T](l: List[T]): List[(T, T)] = l match {
      case fst :: snd :: rest => (fst, snd) :: pairUp(rest)
      case List() => List()
    }

    // maybe later we could make c an implicit parameter
    def genObjectTemplate[C <: Context with Singleton](irs: IRs[C])(ir: irs.ObjectIR): (List[Any], List[irs.FieldIR]) = {
      import irs._
      type Chunked = (List[Any], List[FieldIR])

      debug("genObjectTemplate on "+ ir.tpe.typeSymbol.name)
      debug("fields: " + ir.fields)

      if (ir.fields.isEmpty) {
        val objectChunk = "{\n  \"tpe\": \"" + genTypeTemplate(irs.ctx)(ir.tpe) + "\"\n}"
        (List(objectChunk), List())
      } else {
        // each element in this list is a pair (List[Any], List[FieldIR]) for each field
        // example for one element: (List("name: ", "\n"), List(FieldIR("name")))
        val fieldTemplates: List[Chunked] = ir.fields.map(genFieldTemplate(irs)(_)).map {
          case (List(beginning, end), holes) => (List(",\n" + beginning, end), holes)
        }

        val initialFieldChunks: List[Any] = fieldTemplates.map(_._1).flatten
        debug("initial field chunks: " + initialFieldChunks.mkString("]["))

        val fieldHoles: List[FieldIR] = fieldTemplates.map(_._2).flatten

        val withoutFirstAndLast = initialFieldChunks.tail.init
        debug("withoutFirstAndLast: " + withoutFirstAndLast.mkString("]["))

        val pairs = pairUp(withoutFirstAndLast)
        debug("to be merged: " + pairs.mkString("]["))

        val fieldChunks =
          initialFieldChunks.head +:
          (pairs map { case (left, right) => concatChunked(left, right) }) :+
          initialFieldChunks.last

        debug("field chunks: " + fieldChunks.mkString("]["))

        val objectHeaderChunk: String = "{\n  \"tpe\": \"" + genTypeTemplate(irs.ctx)(ir.tpe) + "\""

        val objectFooterChunk: String = "\n}"

        val firstChunk = concatChunked(objectHeaderChunk, fieldChunks.head)
        val lastChunk  = concatChunked(fieldChunks.last, objectFooterChunk)

        // need to group all chunks and all holes together in separate lists
        val allChunks = List(firstChunk) ++ fieldChunks.tail.init ++ List(lastChunk)
        debug("all chunks: " + allChunks.mkString("]["))
        val allHoles  = fieldHoles
        (allChunks, allHoles)
      }
    }

    def genFieldTemplate[C <: Context with Singleton](irs: IRs[C])(ir: irs.FieldIR): (List[Any], List[irs.FieldIR]) =
      (List("  \"" + ir.name + "\": ", ""), List(ir))

    def concatChunked(c1: Any, c2: Any): Any =
      c1.toString + c2.toString
  }
}


// "{ \"tpe\": \"" + pickleType(c)(tpe) + "\"\n" +
//           genFields(fields).splice(vals) + "\n" +
//           "}"