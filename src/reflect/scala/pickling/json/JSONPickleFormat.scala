/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.pickling

import scala.reflect.runtime.universe._

package json {
  case class JSONPickle(value: String) extends Pickle {
    type Data = String
  }
}

package object json {
  import reflect.macros.Context

  implicit val pickleFormat = new JSONPickleFormat

  class JSONPickleFormat extends PickleFormat {

    // type Pickle = String

    //  // do we need this??
    //  def write(ir: IR): /*Expr[*/Pickle/*]*/ = {
    //    def mkJSONString(ir: IR): String = ir match {
    //      case ObjectIR(tpe, fields) => newObject(tpe, fields.map(fld => newField(fld.name, mkJSONString(fld.value))))
    //      case vir: IR          => vir.value.toString
    //    }
    //    JSONPickle(mkJSONString(ir))
    //  }

    //  // do we need this??
    //  def read(p: Pickle): IR  = ???

    //  def newField(name: String, value: Any): String =  "  \"" + name + "\": \"" + value + "\""
    //  def newObject(tpe: c.Expr[Any], fields: c.Expr[List[String]]): c.Expr[String] =
    //    reify {
    //      "{\n" +
    //      "  \"tpe\": \"" + c.Expr(pickleType).splice + "\",\n" +
    //      fields.mkString(",\n") +
    //      "\n}"
    //    }

    type Data = String

    def pickleType(c: Context)(tpe: c.universe.Type): Data = tpe.toString

    def genObjectTemplate(c: Context)(tpe: c.universe.Type, fields: List[c.Expr[Any => Data]]): c.Expr[List[Any] => Data] = {
      import c.universe._

      def genFields(fldRem: List[Expr[Any => Data]]): Expr[List[Any] => Data] =
        if (fldRem.isEmpty) reify { (vals: List[Any]) => "" }
        else {
          val fstField: Expr[Any => Data] = fldRem.head
          val otherFields: Expr[List[Any] => Data] = genFields(fldRem.tail)
          reify {
            (vals: List[Any]) =>
              fstField.splice(vals.head) +
              otherFields.splice(vals.tail)
          }
        }

      c.universe.reify {
        (vals: List[Any]) =>
          "{ \"tpe\": \"" + pickleType(c)(tpe) + "\"\n" +
          genFields(fields).splice(vals) + "\n" +
          "}"
      }

/*
      reify {
        (vals: List[Any]) =>
          if (fldRem.isEmpty || vals.isEmpty) "" else {
            fldRem.head.splice(vals.head) + "\n" +
            genFields(fldRem.tail).splice(vals.tail)
          }
      }
*/
    }

    def genFieldTemplate(c: Context)(name: String): c.Expr[Any => Data] = c.universe.reify {
      (x: Any) => "\"" + name + "\": \"" + x + "\""
    }

    def build(d: Data): Pickle = new Pickle { val value = d }
  }
}
