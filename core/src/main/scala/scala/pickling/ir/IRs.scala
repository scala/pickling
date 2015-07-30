package scala.pickling
package ir

import scala.reflect.api.Universe
import java.lang.reflect.Modifier

import HasCompat._

class IRs[U <: Universe with Singleton](val uni: U) {
  import uni._
  import compat._
  import definitions._

  // step 1: decide which strategy to use: primary ctor or allocateInstance (pure analysis of the ctor).
  //   - obtain primary ctor (see code)
  //   - go through its params and see if there's a param without a getter.

  // step 2: based on the strategy, we collect the FieldIRs.
  // we should have a method for each strategy.

  sealed abstract class PickleIR

  case class JavaProperty(name: String, declaredIn: String, isSetterPublic: Boolean)

  /* If javaSetter.nonEmpty there is both a getter and a setter method.
   */
  case class FieldIR(name: String, tpe: Type, param: Option[TermSymbol], accessor: Option[MethodSymbol], javaSetter: Option[JavaProperty] = None) {
    def field = accessor.map(_.accessed.asTerm)
    def getter = accessor.map(_.getter).flatMap(sym => if (sym != NoSymbol) Some(sym) else None)
    def setter = accessor.map(_.setter).flatMap(sym => if (sym != NoSymbol) Some(sym) else None)
    def isParam = param.map(_.owner.name == nme.CONSTRUCTOR).getOrElse(false)

    def isPublic = param.nonEmpty || accessor.map(_.isPublic).getOrElse(false)

    // this part is interesting to picklers
    def hasGetter = param.isDefined || getter.isDefined // either case class val or getter

    // this part is interesting to unpicklers
    def hasSetter = setter.isDefined
    def isErasedParam = isParam && accessor.isEmpty // TODO: this should somehow communicate with the constructors phase!
    def isNonParam = !isParam
  }

  case class ClassIR(tpe: Type, parent: ClassIR, fields: List[FieldIR], javaGetInstance: Boolean = false) extends PickleIR {
    var canCallCtor: Boolean = true
  }

  def nonParamFieldIRsOf(tpe: Type): Iterable[FieldIR] = {
    val (quantified, rawTpe) = tpe match { case ExistentialType(quantified, rtpe) => (quantified, rtpe); case rtpe => (Nil, rtpe) }

    val allAccessors = tpe.declarations.collect { case meth: MethodSymbol if meth.isAccessor || meth.isParamAccessor => meth }

    val (filteredAccessors, _) = allAccessors.partition(notMarkedTransient)

    val goodAccessorsNotParams = filteredAccessors.filterNot(_.isParamAccessor)

    val goodAccessorsNotParamsVars = goodAccessorsNotParams.filter(acc => acc.isSetter && acc.accessed != NoSymbol) // 2.10 compat: !acc.isAbstract

    goodAccessorsNotParamsVars.map { symSetter: MethodSymbol =>
      val sym = symSetter.getter.asMethod
      val rawSymTpe = sym.typeSignatureIn(rawTpe) match { case NullaryMethodType(ntpe) => ntpe; case ntpe => ntpe }
      val symTpe = existentialAbstraction(quantified, rawSymTpe)
      FieldIR(sym.name.toString, symTpe, None, Some(sym))
    }
  }

  def nonAbstractVars(tpe: Type, quantified: List[Symbol], rawTpeOfOwner: Type, isJava: Boolean): List[FieldIR] = {
    val javaFieldIRs = if (isJava) {
      // candidates for setter/getter combo
      val candidates = tpe.declarations.collect {
        case sym: MethodSymbol if sym.name.toString.startsWith("get") => sym.name.toString.substring(3)
      }
      tpe.declarations.flatMap {
        case sym: MethodSymbol if sym.name.toString.startsWith("set") =>
          val shortName = sym.name.toString.substring(3)
          if (candidates.find(_ == shortName).nonEmpty && shortName.length > 0) {
            val rawSymTpe = sym.typeSignatureIn(rawTpeOfOwner) match {
              case MethodType(List(param), _) => param.typeSignature
              case _ => throw PicklingException("expected method type for method ${sym.name.toString}")
            }
            val symTpe = existentialAbstraction(quantified, rawSymTpe)

            List(FieldIR(shortName, symTpe, None, None, Some(JavaProperty(shortName, tpe.toString, sym.isPublic))))
          } else {
            List()
          }

        case _ => List()
      }
    } else {
      List()
    }

    (tpe.declarations.collect {
      case sym: MethodSymbol if !sym.isParamAccessor && sym.isSetter && sym.accessed != NoSymbol =>
        val rawSymTpe =
          sym.getter.typeSignatureIn(rawTpeOfOwner) match { case NullaryMethodType(ntpe) => ntpe; case ntpe => ntpe }
        val symTpe =
          existentialAbstraction(quantified, rawSymTpe)
        FieldIR(sym.getter.name.toString, symTpe, None, Some(sym.getter.asMethod))
    }).toList ++ javaFieldIRs
  }

  def newClassIR(tpe: Type): ClassIR = {
    // create new instance of ClassIR(tpe: Type, parent: ClassIR, fields: List[FieldIR])
    // (a) ignore parent (TODO: remove)
    // (b) keep track of canCallCtor (for unpickler generation)

    // idea:
    // param.nonEmpty     iff  field is param of primary ctor
    // accessor.nonEmpty  iff  field has getter and possibly a setter

    val primaryCtor = tpe.declaration(nme.CONSTRUCTOR) match {
      case overloaded: TermSymbol => overloaded.alternatives.head.asMethod // NOTE: primary ctor is always the first in the list
      case primaryCtor: MethodSymbol => primaryCtor
      case NoSymbol => NoSymbol
    }

    // we need all accessors to filter out transient ctor params
    // main diff: members instead of declarations
    val allAccessors = tpe.declarations.collect { case meth: MethodSymbol if meth.isAccessor || meth.isParamAccessor => meth }
    val (filteredAccessors, transientAccessors) = allAccessors.partition(notMarkedTransient)

    val primaryCtorParamsOpt =
      if (primaryCtor.isMethod) Some(primaryCtor.asMethod.paramss.flatten)
      else None

    val canCallCtor =
      primaryCtor != NoSymbol &&
      primaryCtorParamsOpt.nonEmpty &&
      (primaryCtorParamsOpt.get.forall { preSym =>
        // println(s"!!! tpe ${tpe.toString}, ctor param $preSym:")
        val notTransient = !transientAccessors.exists(_.name == preSym.name)
        // println(s"$notTransient")
        if (notTransient) {
          val symOpt = //tpe.declaration(preSym.name)
            filteredAccessors.find(_.name == preSym.name)
          symOpt match {
            case None => false
            case Some(sym) =>
              val isVal = sym.asTerm.isVal
              val getterExists = sym.asTerm.getter != NoSymbol
              // println(s"$isVal (public: ${sym.asTerm.isPublic}, isParamAcc: ${sym.asTerm.isParamAccessor}), $getterExists (${sym.asTerm.getter}, public: ${sym.asTerm.getter.isPublic})")
                (isVal && sym.asTerm.isPublic) || (getterExists && sym.asTerm.getter.isPublic)
          }
        } else false

        // println(s"$notTransient, $isMethod, $getterExists, $getterIsMetod")
        // notTransient && isMethod && getterExists && getterIsMetod
      })

    val (quantified, rawTpe) = tpe match { case ExistentialType(quantified, rtpe) => (quantified, rtpe); case rtpe => (Nil, rtpe) }

    val baseClasses = tpe.typeSymbol.asClass.baseClasses
    // println(s"base classes: ${baseClasses.mkString(",")}")

    def fieldIRsUsingCtor(): List[FieldIR] = {
      // collect:
      // (a) all ctor params
      val ctorFieldIRs = primaryCtorParamsOpt.get.map { s =>
        val sym = s.asTerm // already tested
        val baseSym = if (sym.isVal) sym else sym.getter.asMethod

        val baseSymTpe = baseSym.typeSignature.asSeenFrom(rawTpe, rawTpe.typeSymbol.asClass)
        // println(s"baseSymTpe: ${baseSymTpe.toString}")

        val rawSymTpe = baseSymTpe match { case NullaryMethodType(ntpe) => ntpe; case ntpe => ntpe }
        val symTpe = existentialAbstraction(quantified, rawSymTpe)

        FieldIR(sym.name.toString, symTpe, if (sym.isVal) Some(sym) else None, if (sym.isVal) None else Some(sym.getter.asMethod))
      }

      // (b) non-abstract vars (also private ones)
      val allNonAbstractVars = baseClasses.flatMap { baseClass =>
        nonAbstractVars(tpe.baseType(baseClass), quantified, rawTpe, baseClass.isJava)
      }

      ctorFieldIRs ++ allNonAbstractVars
    }

    def fieldIRsUsingAllocateInstance(): List[FieldIR] = {
      // collect:
      // (a) all vals or vars (even if abstract!!)
      val fieldIRs1 = baseClasses.flatMap { baseClass =>
        val stpe = tpe.baseType(baseClass)
        val allGetters = stpe.declarations.collect {
          case sym: MethodSymbol if sym.isGetter && notMarkedTransient(sym) => sym
        }

        // println(s"allGetters of $baseClass: ${allGetters.mkString(",")}")

        allGetters.map { getter =>
          val rawSymTpe = getter.typeSignatureIn(rawTpe) match { case NullaryMethodType(ntpe) => ntpe; case ntpe => ntpe }
          val symTpe = existentialAbstraction(quantified, rawSymTpe)

          FieldIR(getter.name.toString, symTpe, None, Some(getter))
        }
      }

      val fieldIRs2 = {
        // also add ctor params that are not accessors (need Java reflection for those!)
        val reflectionGetters = {
          if (primaryCtor.isMethod) {
            primaryCtor.asMethod.paramss.flatten.filter { s =>
              val acc = allAccessors.find(_.name == s.name)
              acc.isEmpty
            }
          }
          else List()
        }

        reflectionGetters.map { sym =>
          val rawSymTpe = sym.typeSignatureIn(rawTpe) match { case NullaryMethodType(ntpe) => ntpe; case ntpe => ntpe }
          val symTpe = existentialAbstraction(quantified, rawSymTpe)

          FieldIR(sym.name.toString, symTpe, None, None)
        }
      }

      fieldIRs1 ++ fieldIRs2
    }

    val fieldIRs = if (canCallCtor) {
      val fields = fieldIRsUsingCtor()
      // println(s"fieldIRsUsingCtor of ${tpe.toString}: $fields")
      fields
    } else {
      val fields = fieldIRsUsingAllocateInstance()
      // println(s"fieldIRsUsingAllocateInstance of ${tpe.toString}: $fields")
      fields
    }

    val useGetInstance = if (!(tpe =:= AnyRefTpe) && tpe.typeSymbol.isJava && fieldIRs.isEmpty) {
      val methodOpt =
        try Some(Class.forName(tpe.toString).getDeclaredMethod("getInstance"))
        catch {
          case _: NoSuchMethodException       => None
          case _: ClassNotFoundException      => None
          case _: LinkageError                => None
          case _: ExceptionInInitializerError => None
          case _: SecurityException           => None
        }
      methodOpt.nonEmpty && {
        val mods = methodOpt.get.getModifiers
        Modifier.isStatic(mods) && Modifier.isPublic(mods)
      }
    } else false

    val cir = ClassIR(tpe, null, fieldIRs, useGetInstance)
    cir.canCallCtor = canCallCtor
    cir
  }


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

  private val f2 = (c1: C, c2: C) => ClassIR(c2.tpe, c1, c2.fields)

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
