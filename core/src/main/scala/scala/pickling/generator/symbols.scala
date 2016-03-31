package scala.pickling
package generator

import scala.reflect.api.Universe
import scala.util.Try


/**
 * A minimal symbol set to allow us to construct our mini-pickle-behavior language
 */
private[pickling] sealed trait IrSymbol {}
private[pickling] object IrSymbol {
  def allDeclaredMethodIncludingSubclasses(cls: IrClass): Seq[IrMethod] = {
    def allmethods(clss: List[IrClass], mthds: Seq[IrMethod], visitedClasses: Set[String]): Seq[IrMethod] =
      clss match {
        case hd :: tail if visitedClasses(hd.className) => allmethods(tail, mthds, visitedClasses)
        case hd :: tail =>
          val newMthds = hd.methods
          allmethods(tail ++ hd.parentClasses, newMthds ++ mthds, visitedClasses + hd.className)
        case Nil => mthds
      }
    allmethods(List(cls), Nil, Set())
  }

  def allDeclaredFieldsIncludingSubclasses(cls: IrClass): Seq[IrField] = {
    def allfields(clss: List[IrClass], fields: Seq[IrField], visitedClasses: Set[String]): Seq[IrField] =
      clss match {
        case hd :: tail if visitedClasses(hd.className) => allfields(tail, fields, visitedClasses)
        case hd :: tail =>
          val newFields = hd.fields
          allfields(tail ++ hd.parentClasses, newFields ++ fields, visitedClasses + hd.className)
        case Nil => fields
      }
    allfields(List(cls), Nil, Set())
  }
}
/** Represents a java class. */
private[pickling] trait IrClass extends IrSymbol {
  /** The class name represented by this symbol. */
  def className: String
  /** The primary constructor of the class. */
  def primaryConstructor: Option[IrConstructor]
  /** The methods defined on this class. (May be filtered to only those relevant to pickling interests. */
  def methods: Seq[IrMethod]
  /** The fields on this class.  Note: For scala, these may also show up as methods. */
  def fields: Seq[IrField]
  /** True if this class is defined in Scala. */
  def isScala: Boolean
  /** True if this class is a trait. */
  def isTrait: Boolean
  /** True if this is an abstract class/interface/trait. */
  def isAbstract: Boolean
  /** True if this class is a scala case class. */
  def isCaseClass: Boolean
  /** True if this class is 'final' (or cannot be extended). */
  def isFinal: Boolean
  /** True if this is a scala "object" */
  def isScalaModule: Boolean
  /** Returs all the parent classes (traits, interfaces, etc,) for this type. */
  def parentClasses: Seq[IrClass]
  /** The set of known subclasses for this type.  Will return a failure if the symbol loader
    * isn't sure if the classes are closed.
    */
  def closedSubclasses: Try[Seq[IrClass]]
  /** Return the type of this class for a given universe. */
  def tpe[U <: Universe with Singleton](u: U): u.Type
  /** Returns the companion of this class, if known. */
  def companion: Option[IrClass]
}

private[pickling] sealed trait IrInnerClass extends IrSymbol with IrClass {
  def outerClass: IrClass
}
/** Represents a member of a particular class.
  * This might be a Constructor, Field or Member.
  */
private[pickling] sealed trait IrMember extends IrSymbol {
  /** The class that this member belongs to. */
  def owner: IrClass
  /** Returns true if the members is defined statically for the class. */
  def isStatic: Boolean
  /** Returns true if the given member is publicly accessible. */
  def isPublic: Boolean
  /** True if the field is marked final, and would need to be reflectively set. */
  def isFinal: Boolean
  /** Returns true if the given member is marked as private. */
  def isPrivate: Boolean
  /** Returns true if this member is a scala-symbol (i.e. we make take knowledge of scala-jvm-encoding. */
  def isScala: Boolean
  /** Returns true iff this is an IrField. */
  def isField: Boolean
  /** The name we should use for java reflection. */
  def javaReflectionName: String
  /** This is true if the field is marked transient (either by scala or java serialization). */
  def isMarkedTransient: Boolean
  // TODO - Methods for returning signatures

}
private[pickling] trait IrField extends IrMember {
  /** The name of the field, as we'd use to lookup via reflection. */
  def fieldName: String
  /** Return the type of the field. */
  def tpe[U <: Universe with Singleton](u: U): u.Type

  /** True if the field is a constructor parameter in Scala.  These may not exist at runtime and we don't
    * really know from the symbol table if this is the case.
    */
  def isParameter: Boolean
  override final def isField: Boolean = true
}
private[pickling] trait IrMethod extends IrMember {
  /** The code-names of the constructor parameters.  Note: There is an algorithm which will try to
    * align the constructor parameters with getter methods by symbol name.
    *
    * We do this for case-classes.
    * @return
    */
  def parameterNames: List[List[String]]
  /** Grabs the type of all the constructor parameters. */
  def parameterTypes[U <: Universe with Singleton](u: U): List[List[u.Type]]
  /** The name of the method, as we'd use to lookup via reflection. */
  def methodName: String
  /** Returns true if this method is associated with a var. */
  def isVar: Boolean
  /** Returns true if this method is associated with a scala var. */
  def isVal: Boolean
  /** Returns true if this method is a param accessor for a constructor arg of a class. */
  def isParamAccessor: Boolean
  /** Returns the Scala type associated with this field. */
  def returnType[U <: Universe with Singleton](u: Universe): u.Type

  /** Returns the setter method for this `var` if one exists. */
  def setter: Option[IrMethod]

  override final def isField: Boolean = false
}
/** The symbol representing a constructor. */
private[pickling] trait IrConstructor extends IrMethod {}

/** A symbol loader for Java/Scala Symbols. */
private[pickling] abstract class IrSymbolLoader[U <: Universe with Singleton](val u: U) {
  /** Loads the symbols for a given Type using this symbol loader. */
  def newClass(tpe: u.Type): IrClass
}