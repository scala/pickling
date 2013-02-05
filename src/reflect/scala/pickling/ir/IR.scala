/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.pickling
package ir

import scala.reflect.api.Universe

class IRs(val u: Universe) {
  trait IR

  case class FieldIR(name: String, tpe: u.Type)
  case class ObjectIR(tpe: u.Type, fields: List[FieldIR]) extends IR
}

