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
    def isErasedParam = isParam && accessor.isEmpty // TODO: this should somehow communicate with the constructors phase!
    def isReifiedParam = isParam && accessor.nonEmpty
    def isNonParam = !isParam && field.map(_.isVar).getOrElse(false)
  }
  case class ClassIR(tpe: Type, parent: ClassIR, fields: List[FieldIR]) extends PickleIR

  private type Q = List[FieldIR]
  private type C = ClassIR

  // TODO: minimal versus verbose PickleFormat. i.e. someone might want all concrete inherited fields in their pickle
  private def fields(tp: Type): Q = {
    val info = tp.typeSymbol.typeSignature
    val ctor = info.declaration(nme.CONSTRUCTOR)
    val ctorParams = if (ctor != NoSymbol) ctor.asMethod.paramss.flatten.map(_.asTerm) else Nil // TODO: multiple ctors
    val allAccessors = info.declarations.collect{ case meth: MethodSymbol if meth.isAccessor || meth.isParamAccessor => meth }
    val (paramAccessors, otherAccessors) = allAccessors.partition(_.isParamAccessor)
    def mkFieldIR(sym: TermSymbol, param: Option[TermSymbol], accessor: Option[MethodSymbol]) = FieldIR(sym.name.toString.trim, sym.typeSignatureIn(tp), param, accessor)
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
         .map(_.asType.toType)
         .map(tp => ClassIR(tp, null, fields(tp)))

  private val compose =
    composition(f1, f2, f3)

  private val flatten: C => C = (c: C) =>
    if (c.parent != null) ClassIR(c.tpe, c.parent, f1(c.fields, flatten(c.parent).fields))
    else c

  def classIR(tpe: Type) = flatten(compose(ClassIR(tpe, null, Nil)))
}
