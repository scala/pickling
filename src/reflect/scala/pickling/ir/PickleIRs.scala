package scala.pickling
package ir

import scala.reflect.api.Universe

class PickleIRs[U <: Universe with Singleton](val uni: U) {
  import uni._

  sealed abstract class PickleIR
  case class FieldIR(name: String, tpe: Type)
  case class ClassIR(tpe: Type, parent: ClassIR, fields: List[FieldIR]) extends PickleIR

  type Q = List[FieldIR]
  type C = ClassIR

  def fields(tp: Type): Q =
    tp.typeSymbol
      .typeSignature
      .declarations
      .filter(sym => !sym.isMethod && sym.isTerm && (sym.asTerm.isVar || sym.asTerm.isParamAccessor)) // separate issue: minimal versus verbose PickleFormat . i.e. someone might want all concrete inherited fields in their pickle
      .map(sym => FieldIR(sym.name.toString.trim, sym.typeSignatureIn(tp)))
      .toList

  def composition(f1: (Q, Q) => Q, f2: (C, C) => C, f3: C => List[C]) =
    (c: C) => f3(c).reverse.reduce[C](f2)

  val f1 = (q1: Q, q2: Q) => q1 ++ q2

  val f2 = (c1: C, c2: C) => ClassIR(c2.tpe, c1, fields(c2.tpe))

  val f3 = (c: C) =>
    c.tpe.baseClasses
         .map(_.asType.toType)
         .map(tp => ClassIR(tp, null, fields(tp)))

  val compose =
    composition(f1, f2, f3)

  val flatten: C => C = (c: C) =>
    if (c.parent != null) ClassIR(c.tpe, c.parent, f1(c.fields, flatten(c.parent).fields))
    else c
}
