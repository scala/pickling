package scala.pickling
package ir

import scala.reflect.api.Universe

class IRs[U <: Universe with Singleton](val uni: U) {
  import uni._
  import definitions._

  sealed abstract class PickleIR
  case class FieldIR(name: String, tpe: Type, param: Option[TermSymbol], accessor: Option[MethodSymbol]) {
    def field = accessor.map(_.accessed.asTerm)
    def getter = accessor.map(_.getter).flatMap(sym => if (sym != NoSymbol) Some(sym) else None)
    def setter = accessor.map(_.setter).flatMap(sym => if (sym != NoSymbol) Some(sym) else None)
    def isParam = param.map(_.owner.name == nme.CONSTRUCTOR).getOrElse(false)
    def isPublic = accessor.map(_.isPublic).getOrElse(false)

    // this part is interesting to picklers
    def hasGetter = getter.isDefined

    // this part is interesting to unpicklers
    def hasSetter = setter.isDefined
    def isErasedParam = isParam && accessor.isEmpty // TODO: this should somehow communicate with the constructors phase!
    def isReifiedParam = isParam && accessor.nonEmpty
    def isNonParam = !isParam
  }
  case class ClassIR(tpe: Type, parent: ClassIR, fields: List[FieldIR]) extends PickleIR

  private type Q = List[FieldIR]
  private type C = ClassIR

  // TODO: minimal versus verbose PickleFormat. i.e. someone might want all concrete inherited fields in their pickle
  private def fields(tp: Type): Q = {
    val ctor = tp.declaration(nme.CONSTRUCTOR) match {
      case overloaded: TermSymbol => overloaded.alternatives.head.asMethod // NOTE: primary ctor is always the first in the list
      case primaryCtor: MethodSymbol => primaryCtor
      case NoSymbol => NoSymbol
    }
    val ctorParams = if (ctor != NoSymbol) ctor.asMethod.paramss.flatten.map(_.asTerm) else Nil
    val allAccessors = tp.declarations.collect{ case meth: MethodSymbol if meth.isAccessor || meth.isParamAccessor => meth }
    val (paramAccessors, otherAccessors) = allAccessors.partition(_.isParamAccessor)

    def mkFieldIR(sym: TermSymbol, param: Option[TermSymbol], accessor: Option[MethodSymbol]) = {
      val (quantified, rawTp) = tp match { case ExistentialType(quantified, tpe) => (quantified, tpe); case tpe => (Nil, tpe) }
      val rawSymTp = accessor.getOrElse(sym).typeSignatureIn(rawTp) match { case NullaryMethodType(tpe) => tpe; case tpe => tpe }
      val symTp = existentialAbstraction(quantified, rawSymTp)
      FieldIR(sym.name.toString.trim, symTp, param, accessor)
    }

    val paramFields = ctorParams.map(sym => mkFieldIR(sym, Some(sym), paramAccessors.find(_.name == sym.name)))
    val varGetters = otherAccessors.collect{ case meth if meth.isGetter && meth.accessed != NoSymbol && meth.accessed.asTerm.isVar => meth }
    val varFields = varGetters.map(sym => mkFieldIR(sym, None, Some(sym)))

    paramFields ++ varFields
  }

  private def composition(f1: (Q, Q) => Q, f2: (C, C) => C, f3: C => List[C]) =
    (c: C) => f3(c).reverse.reduce[C](f2)

  private val f1 = (q1: Q, q2: Q) => q1 ++ q2

  private val f2 = (c1: C, c2: C) => ClassIR(c2.tpe, c1, fields(c2.tpe))

  private val f3 = (c: C) =>
    c.tpe.baseClasses
         .map(superSym => c.tpe.baseType(superSym))
         .map(tp => ClassIR(tp, null, fields(tp)))

  private val compose =
    composition(f1, f2, f3)

  private val flatten: C => C = (c: C) =>
    if (c.parent != null) ClassIR(c.tpe, c.parent, f1(c.fields, flatten(c.parent).fields))
    else c

  def flattenedClassIR(tpe: Type) = flatten(compose(ClassIR(tpe, null, Nil)))
}
