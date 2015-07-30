package scala.pickling
package ir


import HasCompat._
import scala.reflect.api.Universe

/** An implementation of a symbol lookup table based on scala reflection.
  *
  *
  * Note:  Scala reflection/compiler have known inaccuracies in some lookups.  For example, the compiler may
  *        remove all knowledge of private symbols from Java classes, because generally it doesn't care about
  *        Java's privates.
  */
class IrScalaSymbols[U <: Universe with Singleton](override val uni: U) extends IrSymbolLoader[U](uni) {
  import uni._
  import compat._
  import definitions._


  def newClass(tpe: Type): IrClass = new ScalaIrClass(tpe)

  // Implementation of IrClass symbol using a scala Type.
  private class ScalaIrClass(tpe: Type) extends IrClass {
    private val (quantified, rawTpe) = tpe match { case ExistentialType(quantified, rtpe) => (quantified, rtpe); case rtpe => (Nil, rtpe) }

    private def classSymbol = tpe.typeSymbol.asClass
    /** The class name represented by this symbol. */
    override def className: String = classSymbol.fullName
    override def primaryConstructor: Option[IrConstructor] = {
      tpe.declaration(nme.CONSTRUCTOR) match {
        // NOTE: primary ctor is always the first in the list
        case overloaded: TermSymbol => Some(new ScalaIrConstructor(overloaded.alternatives.head.asMethod, this))
        case primaryCtor: MethodSymbol => Some(new ScalaIrConstructor(primaryCtor, this))
        case NoSymbol => None
      }
    }
    // The list of all "accessor" method symbols of the class.  We filter to only these to avoid ahving too many symbols
    private val allAccessors = tpe.declarations.collect { case meth: MethodSymbol if meth.isAccessor || meth.isParamAccessor => meth }
    // Here we only return "accessor" methods.
    override val methods: Seq[IrMethod] = {
      (allAccessors map { mth =>
        new ScalaIrMethod(mth, this)
      })(collection.breakOut)
    }

    override def isScala = !tpe.typeSymbol.isJava

    /** True if this class is a scala case class. */
    override def isCaseClass: Boolean = classSymbol.isCaseClass

    /** True if this class is 'final' (or cannot be extended). */
    override def isFinal: Boolean = classSymbol.isFinal
  }
  private class ScalaIrMethod(mthd: MethodSymbol, override val owner: IrClass) extends IrMethod {
    override def methodName: String = mthd.name.toString
    // TODO - Figure out if the method is JVM public or not.
    override def isPublic: Boolean = mthd.isPublic
    override def isStatic: Boolean = mthd.isStatic
    override def toString = s"def ${methodName}: ${mthd.typeSignature}"
    override def isVar: Boolean =
      (mthd.getter != NoSymbol) && (mthd.setter != NoSymbol)
    override def returnType[U <: Universe with Singleton](u: Universe): u.Type = mthd.returnType.asInstanceOf[u.Type]

  }

  private class ScalaIrConstructor(mthd: MethodSymbol, owner: IrClass) extends ScalaIrMethod(mthd, owner) with IrConstructor {
    override def parameterNames: Seq[String] =
      mthd.paramss.flatten.map(_.name.toString) // TODO - Is this safe?

    override def toString = s"CONSTRUCTOR ${owner.className} (${parameterNames.mkString(",")}}): ${mthd.typeSignature}"
  }
}