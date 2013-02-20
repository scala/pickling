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


class IRs[C <: Context with Singleton](val ctx: C) {
  import ctx._

  trait IR
  case class FieldIR(name: String, tpe: Type)
  case class ObjectIR(tpe: Type, parent: ObjectIR, fields: List[FieldIR]) extends IR

  type Q = List[FieldIR]
  type C = ObjectIR

  def fields(tp: Type): Q =
    tp.declarations
      .filter(sym => !sym.isMethod)
      .map(sym => FieldIR(sym.name.toString.trim, sym.typeSignatureIn(tp)))
      .toList

  def composition(f1: (Q, Q) => Q, f2: (C, C) => C, f3: C => List[C]) =
    (c: C) => f3(c).reverse.reduce[C](f2)

  val f1 = (q1: Q, q2: Q) => q1 ++ q2

  val f2 = (c1: C, c2: C) => ObjectIR(c2.tpe, c1, fields(c2.tpe))

  val f3 = (c: C) =>
    c.tpe.baseClasses
         .map(_.typeSignature)
         .map(tp => ObjectIR(tp, null, fields(tp)))

  val compose =
    composition(f1, f2, f3)

  val flatten: C => C = (c: C) =>
    if (c.parent != null) ObjectIR(c.tpe, c.parent, f1(c.fields, flatten(c.parent).fields))
    else c
}
