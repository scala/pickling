package scala.pickling
package generator


import HasCompat._
import scala.reflect.api.Universe
import scala.reflect.macros.Context


private[pickling] class UnclosedSubclassesException(errors: Seq[String]) extends Exception(errors.mkString("\n")) {
  override def fillInStackTrace(): Throwable = this
}

/** An implementation of a symbol lookup table based on scala reflection.
  *
  *
  * Note:  Scala reflection/compiler have known inaccuracies in some lookups.  For example, the compiler may
  *        remove all knowledge of private symbols from Java classes, because generally it doesn't care about
  *        Java's privates.
  */
private[pickling] class IrScalaSymbols[U <: Universe with Singleton, C <: Context](override val u: U, tools: Tools[C]) extends IrSymbolLoader[U](u) {
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

  def newClass(tpe: Type): IrClass =
    if(tpe.typeSymbol.isClass) {
      val (quantified, rawTpe) = tpe match { case ExistentialType(quantified, rtpe) => (quantified, rtpe); case rtpe => (Nil, rtpe) }
      new ScalaIrClass(tpe, quantified, rawTpe)
    } else sys.error(s"Don't know how to handle $tpe, ${tpe.typeSymbol.owner}")



  // Implementation of IrClass symbol using a scala Type.
  private class ScalaIrClass(private[generator] val tpe: Type) extends IrClass {
    //System.err.println(s"New class: $tpe")
    def this(tpe: Type, quantified: List[Symbol], rawType: Type) = this(tpe)
    private[generator] val (quantified, rawType) = tpe match { case ExistentialType(quantified, rtpe) => (quantified, rtpe); case rtpe => (Nil, rtpe) }
    private def classSymbol = tpe.typeSymbol.asClass
    /** The class name represented by this symbol. */
    override def className: String = classSymbol.fullName
    override def isTrait: Boolean = classSymbol.isTrait
    override def isAbstract: Boolean = {
      classSymbol.isAbstractType
      // NOte: This doesn't exist on scala 2.10
      //classSymbol.isAbstract
      classSymbol.isAbstractClass
    }
    override def primaryConstructor: Option[IrConstructor] = {
      tpe.declaration(nme.CONSTRUCTOR) match {
        // NOTE: primary ctor is always the first in the list
        case overloaded: TermSymbol => Some(new ScalaIrConstructor(overloaded.alternatives.head.asMethod, this))
        case primaryCtor: MethodSymbol => Some(new ScalaIrConstructor(primaryCtor, this))
        case NoSymbol => None
      }
    }
    private val allMethods = {
      val constructorArgs = tpe.members.collect {
        case meth: MethodSymbol => meth
      }.toList.filter { x =>
        //System.err.println(s"$x - param: ${x.isParamAccessor}, var: ${x.isVar}, val: ${x.isVal}, owner: ${x.owner}, owner-constructor: ${x.owner.isConstructor}")
        (x.owner == tpe.typeSymbol) && (x.isParamAccessor)
      }.toList
      //System.err.println(s"$tpe has constructor args:\n - ${constructorArgs.mkString("\n - ")}")
      // NOTE - This will only collect memeber vals/vals.  It's possible some things come from the constructor.
      val declaredVars = (tpe.declarations).collect { case meth: MethodSymbol => meth }.toList
      // NOTE - There can be duplication between 'constructor args' and 'declared vars' we'd like to avoid.
        declaredVars
    }
    // The list of all "accessor" method symbols of the class.  We filter to only these to avoid ahving too many symbols
    //private val fields? = allMethods.collect { case meth: MethodSymbol if meth.isAccessor || meth.isParamAccessor => meth }

    // Here we only return "accessor" methods.
    override val methods: Seq[IrMethod] = {
      (allMethods map { mth =>
        new ScalaIrMethod(mth, this)
      })(collection.breakOut)
    }
    override def fields: Seq[IrField] = {
      // TODO - It's possible some terms come from the constructor.  We don't really know if they are available at runtime
      //        or not, so we may ignore them.
      //        It's actually a really bad scenario because you can't distinguish between something which
      //        is actually annotated as a val and something which is just a constructor argument.
      def isConstructorArg(x: TermSymbol): Boolean = {
        // Note available in scala 2.10
        // x.owner.isConstructor
        x.owner.name == nme.CONSTRUCTOR
      }
      tpe.members.filter(_.isTerm).map(_.asTerm).filter(x => x.isVal || x.isVar).map(x => new ScalaIrField(x, this)).toList
    }
    override def companion: Option[IrClass] = {
      if(tpe.typeSymbol.isType) {
        val tmp = tpe.typeSymbol.asType.companionSymbol
        if(tmp.isType) {
          val cp = tmp.asType.toType
          if (cp != NoType) Some(new ScalaIrClass(cp, quantified, rawType))
          else None
        }
        else None
      } else None
    }

    override def isScala = !tpe.typeSymbol.isJava
    override def isScalaModule: Boolean = classSymbol.isModuleClass

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
            dispatchees.map(t => new ScalaIrClass(t.asInstanceOf[u.Type], quantified, rawType))(collection.breakOut)
          })
        case errors =>
          scala.util.Failure(new UnclosedSubclassesException(errors))
      }
    }
    // TODO - use tpe.key implicit from helpers to get a consistent tag key here.
    override def toString = s"$tpe"

    /** Returs all the parent classes (traits, interfaces, etc,) for this type. */
    override def parentClasses: Seq[IrClass] = {
      // We always drop the first class, becasue it is ourself.
      classSymbol.baseClasses.drop(1) filter (_.isClass) map { x =>
        //new ScalaIrClass(fillParameters(x), quantified, rawType)
        new ScalaIrClass(tpe.baseType(x), quantified, rawType)
      }
    }

    /** Fill is the concrete types for a given symbol using the concrete types this class knows about. */
    final def fillParameters(baseSym: Symbol): Type = {
      //System.err.println(s"baseSym= ${baseSym.toString}")
      val baseSymTpe = baseSym.typeSignature.asSeenFrom(rawType, rawType.typeSymbol.asClass)
      //System.err.println(s"baseSymTpe: ${baseSymTpe.toString}")

      val rawSymTpe = baseSymTpe match { case NullaryMethodType(ntpe) => ntpe; case ntpe => ntpe }
      val result = existentialAbstraction(quantified, rawSymTpe)
      //System.err.println(s"result = ${result.toString}")
      result

    }
    /** This is part of a workaround for issues discovering transient annotations on fields. */
    private[IrScalaSymbols] val transientArgNames: Set[String] = {
      IrSymbol.allDeclaredMethodIncludingSubclasses(this).filter(x => x.isParamAccessor || x.isVar || x.isVal).filter(_.isMarkedTransient).map(_.methodName).toSet
    }

  }

  private class ScalaIrField(field: TermSymbol, override val owner: ScalaIrClass) extends IrField{

    override def isMarkedTransient: Boolean = {
      val tr = scala.util.Try {
        ((field.accessed != NoSymbol) && field.accessed.annotations.exists(_.tpe =:= typeOf[scala.transient])) ||
        ((field.getter != NoSymbol) && field.getter.annotations.exists(_.tpe =:= typeOf[scala.transient])) ||
          (field.annotations.exists(_.tpe =:= typeOf[scala.transient]))
      }
      // Here we wrokaround a scala symbol issue where the field is never annotated with transient.
      val isSameNameAsTransientVar = owner.transientArgNames(fieldName)
      isSameNameAsTransientVar || tr.getOrElse(false)
    }

    private def removeTrailingSpace(orig: String): String =
      orig match {
        // TODO - Why do we need this random fix, is this a bug?
        case x if x endsWith " " =>
          //System.err.println(s"Caugh funny symbol: $field, name: ${field.name}, fullName: ${field.fullName}")
          x.dropRight(1).toString
        case x => x
      }
    override def fieldName: String = removeTrailingSpace(field.name.toString)
    override def tpe[U <: Universe with Singleton](u: U): u.Type = field.typeSignature.asSeenFrom(owner.tpe, owner.tpe.typeSymbol).asInstanceOf[u.Type]
    override def isPublic: Boolean = field.isPublic
    override def isStatic: Boolean = field.isStatic
    override def isFinal: Boolean = field.isFinal
    override def isScala: Boolean = true // We don't generate fields for java types
    override def isPrivate: Boolean = field.isPrivate
    override def isParameter: Boolean = field.isParameter
    // TODO - isPrivateThis

    // TODO - We want to make sure this name matches what we'll see in reflection.
    override def toString = s"field ${fieldName}: ${field.typeSignature}"

    // TODO - some kind of check to see if the field actually exists in runtime.  Scala sometimes erases
    //        fields completely, and having a field is actually more of a runtime concern for scala.
    //        Unforutnately, this TODO may be impossible.
    override def javaReflectionName: String =
      removeTrailingSpace(field.name.toString)
  }

  private class ScalaIrMethod(mthd: MethodSymbol, override val owner: ScalaIrClass) extends IrMethod {
    import owner.fillParameters
    override def parameterNames: List[List[String]] =
      mthd.paramss.map(_.map(_.name.toString))

    override def parameterTypes[U <: Universe with Singleton](u: U): List[List[u.Type]] = {
      mthd.paramss.map(_.map(x => fillParameters(x).asSeenFrom(owner.tpe, owner.tpe.typeSymbol)).map(_.asInstanceOf[u.Type]))
    }

    override def isMarkedTransient: Boolean = {
      // TODO - is this correct?
      val tr = scala.util.Try {
        ((mthd.accessed != NoSymbol) && mthd.accessed.annotations.exists(_.tpe =:= typeOf[scala.transient])) ||
          ((mthd.getter != NoSymbol) && mthd.getter.annotations.exists(_.tpe =:= typeOf[scala.transient])) ||
          (mthd.annotations.exists(_.tpe =:= typeOf[scala.transient]))
      }
      tr.getOrElse(false)
    }

    override def methodName: String = {
      mthd.name.toString match {
        // TODO - Why do we need this random fix, is this a bug?
        case x if x endsWith " " => x.dropRight(1).toString
        case x => x
      }
    }
    override def javaReflectionName: String = {
      val isPrivateThis = {
        // Note: Scala 2.10 does not support this
        //mthd.isPrivateThis
        false
      }
      if(mthd.isParamAccessor && (mthd.isPrivate || isPrivateThis)) {
        // Here we check to see if we need to encode the funky name that scala gives private fields to avoid conflicts
        // with fields in the parent class.
        def makeEncodedJvmName(names: List[String], buf: StringBuilder, isStart: Boolean = false): String =
           names match {
             case Nil => buf.toString
             case next :: rest if isStart =>
               buf.append(next)
               makeEncodedJvmName(rest, buf, isStart = false)
             case next :: Nil =>
               buf.append("$$").append(next)
               buf.toString
             case next :: rest =>
               buf.append("$").append(next)
               makeEncodedJvmName(rest, buf, isStart = false)
           }
        val split = mthd.fullName.split('.').toList
        val result = makeEncodedJvmName(split, new StringBuilder, true)
        result
      } else mthd match {
          // Note: Not available in Scala 2.10.x
        //case TermName(n) => n
        case x: TermName => x.toString
        case _ => mthd.name.encodedName.toString
      }
    }
    // TODO - Figure out if the method is JVM public or not (different than scala public)
    override def isPublic: Boolean = mthd.isPublic
    override def isStatic: Boolean = mthd.isStatic
    override def isFinal: Boolean = mthd.isFinal
    override def isPrivate: Boolean = mthd.isPrivate
    override def isScala: Boolean = !mthd.isJava
    override def toString = s"def ${methodName}: ${mthd.typeSignature}"
    override def isParamAccessor: Boolean = mthd.isParamAccessor
    override def isVal: Boolean = mthd.isVal
    override def isVar: Boolean =
      (mthd.getter != NoSymbol) && (mthd.setter != NoSymbol) &&
        (mthd.setter != mthd) // THis is  hack so the setter doesn't show up in our list of vars.
    override def returnType[U <: Universe with Singleton](u: Universe): u.Type =
       mthd.returnType.asSeenFrom(owner.tpe, owner.tpe.typeSymbol).asInstanceOf[u.Type]
    override def setter: Option[IrMethod] = {
      mthd.setter match {
        case NoSymbol => None
        case x => Some(new ScalaIrMethod(x.asMethod, owner))
      }
    }
  }

  private class ScalaIrConstructor(mthd: MethodSymbol, owner: ScalaIrClass) extends ScalaIrMethod(mthd, owner) with IrConstructor {

    override def returnType[U <: Universe with Singleton](u: Universe): u.Type = owner.tpe[u.type](u)
    override def toString = s"CONSTRUCTOR ${owner} (${parameterNames.mkString(",")}}): ${mthd.typeSignature}"
  }
}