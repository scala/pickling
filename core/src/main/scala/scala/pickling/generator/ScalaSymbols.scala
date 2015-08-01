package scala.pickling
package generator


import HasCompat._
import scala.reflect.api.Universe
import scala.reflect.macros.Context


class UnclosedSubclassesException(errors: Seq[String]) extends Exception(errors.mkString("\n")) {
  override def fillInStackTrace(): Throwable = this
}

/** An implementation of a symbol lookup table based on scala reflection.
  *
  *
  * Note:  Scala reflection/compiler have known inaccuracies in some lookups.  For example, the compiler may
  *        remove all knowledge of private symbols from Java classes, because generally it doesn't care about
  *        Java's privates.
  */
class IrScalaSymbols[U <: Universe with Singleton, C <: Context](override val u: U, tools: Tools[C]) extends IrSymbolLoader[U](u) {
  import u._
  import compat._
  import definitions._
  import tools._

  import scala.pickling.internal.RichSymbol

  // HELPER METHODS FOR SUBCLASSES
  def isCaseClass(sym: TypeSymbol): Boolean =
    sym.isClass && sym.asClass.isCaseClass

  def isClosed(sym: tools.u.TypeSymbol): Boolean =
    whyNotClosed(sym).isEmpty

  def whyNotClosed(sym: tools.u.TypeSymbol): Seq[String] = {
    if (sym.isEffectivelyFinal)
      Nil
    else if (isCaseClass(sym.asInstanceOf[u.TypeSymbol]))
      Nil
    else if (sym.isClass) {
      val classSym = sym.asClass
      if (tools.treatAsSealed(classSym)) {
        tools.directSubclasses(classSym).flatMap(cl => whyNotClosed(cl.asType))
      } else {
        List(s"'${sym.fullName}' allows unknown subclasses (it is not sealed or final isCaseClass=${isCaseClass(sym.asInstanceOf[u.TypeSymbol])} isEffectivelyFinal=${sym.isEffectivelyFinal} isSealed=${classSym.isSealed} directSubclasses=${tools.directSubclasses(classSym)})")
      }
    } else {
      List(s"'${sym.fullName}' is not a class or trait")
    }
  }

  def newClass(tpe: Type): IrClass = new ScalaIrClass(tpe)

  // Implementation of IrClass symbol using a scala Type.
  private class ScalaIrClass(tpe: Type) extends IrClass {
    private val (quantified, rawTpe) = tpe match { case ExistentialType(quantified, rtpe) => (quantified, rtpe); case rtpe => (Nil, rtpe) }

    private def classSymbol = tpe.typeSymbol.asClass
    /** The class name represented by this symbol. */
    override def className: String = classSymbol.fullName
    override def isTrait: Boolean = classSymbol.isTrait
    override def isAbstract: Boolean = classSymbol.isAbstract
    override def primaryConstructor: Option[IrConstructor] = {
      tpe.declaration(nme.CONSTRUCTOR) match {
        // NOTE: primary ctor is always the first in the list
        case overloaded: TermSymbol => Some(new ScalaIrConstructor(overloaded.alternatives.head.asMethod, this))
        case primaryCtor: MethodSymbol => Some(new ScalaIrConstructor(primaryCtor, this))
        case NoSymbol => None
      }
    }

    private val allMethods = tpe.declarations.collect { case meth: MethodSymbol => meth }
    // The list of all "accessor" method symbols of the class.  We filter to only these to avoid ahving too many symbols
    //private val allAccessors = allMethods.collect { case meth: MethodSymbol if meth.isAccessor || meth.isParamAccessor => meth }
    // Here we only return "accessor" methods.
    override val methods: Seq[IrMethod] = {
      (allMethods map { mth =>
        new ScalaIrMethod(mth, this)
      })(collection.breakOut)
    }
    override def companion: Option[IrClass] = {
      if(tpe.typeSymbol.isType) {
        val tmp = tpe.typeSymbol.asType.companionSymbol
        if(tmp.isType) {
          val cp = tmp.asType.toType
          if (cp != NoType) Some(new ScalaIrClass(cp))
          else None
        }
        else None
      } else None
    }

    override def isScala = !tpe.typeSymbol.isJava
    override def isScalaModule: Boolean = tpe.typeSymbol.isModule || tpe.typeSymbol.isModuleClass

    /** True if this class is a scala case class. */
    override def isCaseClass: Boolean = classSymbol.isCaseClass

    /** True if this class is 'final' (or cannot be extended). */
    override def isFinal: Boolean = classSymbol.isFinal
    override def tpe[U <: Universe with Singleton](u: U): u.Type = tpe.asInstanceOf[u.Type]



    /** The set of known subclasses for this type.  May be empty if we don't know of any.
      * Note:  This method will return a failure if the known subclasses aren't "closed"
      */
    override def closedSubclasses: scala.util.Try[Seq[IrClass]] = {
      val closedError = whyNotClosed(tpe.typeSymbol.asType.asInstanceOf[tools.u.TypeSymbol])
      closedError match {
        case Nil =>
          scala.util.Success({
            val dispatchees = tools.compileTimeDispatchees(tpe.asInstanceOf[tools.c.universe.Type], tools.u.rootMirror, false)
            dispatchees.map(t => new ScalaIrClass(t.asInstanceOf[u.Type]))(collection.breakOut)
          })
        case errors =>
          scala.util.Failure(new UnclosedSubclassesException(errors))
      }
    }
    // TODO - use tpe.key implicit from helpers to get a consistent tag key here.
    override def toString = s"$tpe"
  }
  private class ScalaIrMethod(mthd: MethodSymbol, override val owner: IrClass) extends IrMethod {
    override def parameterNames: List[List[String]] =
      mthd.paramss.map(_.map(_.name.toString)) // TODO - Is this safe?

    override def parameterTypes[U <: Universe with Singleton](u: U): List[List[u.Type]] = {
      mthd.paramss.map(_.map(_.typeSignature.asInstanceOf[u.Type]))
    }
    // TODO - We need to get the actual jvm name here.
    override def methodName: String = mthd.name.toString
    override def javaReflectionName: String = {
      mthd match {
        case TermName(n) => n
        case _ => mthd.name.toString
      }
    }
    // TODO - Figure out if the method is JVM public or not.
    override def isPublic: Boolean = mthd.isPublic
    override def isStatic: Boolean = mthd.isStatic
    override def isPrivate: Boolean = mthd.isPrivate
    override def isScala: Boolean = !mthd.isJava
    override def toString = s"def ${methodName}: ${mthd.typeSignature}"
    override def isVar: Boolean =
      (mthd.getter != NoSymbol) && (mthd.setter != NoSymbol) &&
        (mthd.setter != mthd) // THis is  hack so the setter doesn't show up in our list of vars.
    override def returnType[U <: Universe with Singleton](u: Universe): u.Type = mthd.returnType.asInstanceOf[u.Type]
    override def setter: Option[IrMethod] = {
      mthd.setter match {
        case NoSymbol => None
        case x => Some(new ScalaIrMethod(x.asMethod, owner))
      }
    }
  }

  private class ScalaIrConstructor(mthd: MethodSymbol, owner: IrClass) extends ScalaIrMethod(mthd, owner) with IrConstructor {

    override def toString = s"CONSTRUCTOR ${owner} (${parameterNames.mkString(",")}}): ${mthd.typeSignature}"
  }
}