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
  import ir._
  import reflect.macros.Context

  implicit val pickleFormat = new JSONPickleFormat

  class JSONPickleFormat extends PickleFormat {

     // do we need this??
     def write(ir: IR): /*Expr[*/Pickle/*]*/ = {
       def mkJSONString(ir: IR): String = ir match {
         case ObjectIR(tpe, fields) => newObject(tpe, fields.map(fld => newField(fld.name, mkJSONString(fld.value))))
         case vir: IR          => vir.value.toString
       }
       JSONPickle(mkJSONString(ir))
     }

     // do we need this??
     def read(p: Pickle): IR  = ???

     // do we need this??
     def pickleType(c: Context)(tpe: c.universe.Type): Any = tpe.toString

     def newField(name: String, value: Any): String =  "  \"" + name + "\": \"" + value + "\""
     def newObject(tpe: c.Expr[Any], fields: c.Expr[List[String]]): c.Expr[String] =
       reify {
         "{\n" +
         "  \"tpe\": \"" + c.Expr(pickleType).splice + "\",\n" +
         fields.mkString(",\n") +
         "\n}"
       }
  }
}
