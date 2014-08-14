package scala.pickling
package ir

import scala.reflect.api.Universe

import HasCompat._

class IRs[U <: Universe with Singleton](val uni: U) {
  import uni._
  import compat._
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
    def isNonParam = !isParam
  }
  case class ClassIR(tpe: Type, parent: ClassIR, fields: List[FieldIR]) extends PickleIR

  private type Q = List[FieldIR]
  private type C = ClassIR

  // TODO: minimal versus verbose PickleFormat. i.e. someone might want all concrete inherited fields in their pickle

  def notMarkedTransient(sym: TermSymbol): Boolean = {
    val tr = scala.util.Try {
      (sym.accessed == NoSymbol) || // if there is no backing field, then it cannot be marked transient
      !sym.accessed.annotations.exists(_.tpe =:= typeOf[scala.transient])
    }
    tr.isFailure || tr.get
  }

  /** Creates FieldIRs for the given type, tp.
  */
  private def fields(tp: Type): Q = {
    println(s"computing fields for ${tp.toString}")

    val ctor = tp.declaration(nme.CONSTRUCTOR) match {
      case overloaded: TermSymbol => overloaded.alternatives.head.asMethod // NOTE: primary ctor is always the first in the list
      case primaryCtor: MethodSymbol => primaryCtor
      case NoSymbol => NoSymbol
    }

    val allAccessors = tp.declarations.collect { case meth: MethodSymbol if meth.isAccessor || meth.isParamAccessor => meth }

    val (filteredAccessors, transientAccessors) = allAccessors.partition(notMarkedTransient)

    val ctorParams = if (ctor != NoSymbol) ctor.asMethod.paramss.flatten.flatMap { sym =>
      if (transientAccessors.exists(acc => acc.name.toString == sym.name.toString)) List()
      else List(sym.asTerm)
    } else Nil

    val (paramAccessors, otherAccessors) = allAccessors.partition(_.isParamAccessor)

    def mkFieldIR(sym: TermSymbol, param: Option[TermSymbol], accessor: Option[MethodSymbol]) = {
      /*
      val (quantified, rawTp) = tp match { case ExistentialType(quantified, tpe) => (quantified, tpe); case tpe => (Nil, tpe) }
      val rawSymTp = accessor.getOrElse(sym).typeSignatureIn(rawTp) match { case NullaryMethodType(tpe) => tpe; case tpe => tpe }
      val symTp = existentialAbstraction(quantified, rawSymTp)
      */
      val symTp = tp match {
        case ExistentialType(quantified, rawTpe) =>
          println(s"computing fields of existential type ${tp.toString}")
          ???
        case _ =>
          accessor.getOrElse(sym).typeSignatureIn(tp) match {
            case NullaryMethodType(tpe) => tpe; case tpe => tpe
          }
      }

      FieldIR(sym.name.toString.trim, symTp, param, accessor)
    }

    val paramFields = ctorParams.map { sym =>
      println(s"real info about ctor parm $sym:")

      val classSym = tp.typeSymbol.asClass
      println(s"tp classSym: $classSym")
      val paramClassSym = sym.typeSignature.typeSymbol.asClass
      println(s"param classSym: $paramClassSym")

      println(s"sym.typeSignature: ${sym.typeSignature}")

      println(s"sym.getter: ${sym.getter}")
      val paramTpe = sym.typeSignatureIn(tp)
      println(s"type: $paramTpe")

      mkFieldIR(sym, Some(sym), paramAccessors.find(_.name == sym.name))
    }
    val varGetters = otherAccessors.collect{ case meth if meth.isGetter && meth.accessed != NoSymbol && meth.accessed.asTerm.isVar => meth }
    val varFields = varGetters.map(sym => mkFieldIR(sym, None, Some(sym)))

    println(s"paramFields: $paramFields.mkString(",")")
    println(s"varFields: $varFields.mkString(",")")

    paramFields ++ varFields
  }

  private def composition(f1: (Q, Q) => Q, f2: (C, C) => C, f3: C => List[C]) =
    (c: C) => f3(c).reverse.reduce[C](f2)

  private val f1 = (q1: Q, q2: Q) => q1 ++ q2

  private val f2 = (c1: C, c2: C) => ClassIR(c2.tpe, c1, c2.fields)

  private val f3 = (c: C) =>
    c.tpe.baseClasses
         .map(superSym => c.tpe.baseType(superSym).asSeenFrom(c.tpe, superSym))
         .map(tp => ClassIR(tp, null, fields(tp)))

  private val compose =
    composition(f1, f2, f3)

  private val flatten: C => C = (c: C) =>
    if (c.parent != null) ClassIR(c.tpe, c.parent, f1(c.fields, flatten(c.parent).fields))
    else c

  def flattenedClassIR(tpe: Type) = flatten(compose(ClassIR(tpe, null, Nil)))
}
