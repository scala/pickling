/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.pickling
package ir

import scala.reflect.macros.Context

class IRs[C <: Context](val c: C) {
  trait IR

  case class FieldIR(name: String, tpe: c.universe.Type)
  case class ObjectIR(tpe: c.universe.Type, fields: List[FieldIR]) extends IR
}

