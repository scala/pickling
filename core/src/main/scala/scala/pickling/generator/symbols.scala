package scala.pickling
package generator

import scala.reflect.api.Universe
import scala.util.Try


/**
 * A minimal symbol set to allow us to construct our mini-pickle-behavior language
 */
sealed trait IrSymbol {
  // TODO - isJava
}
/** Represents a java class. */
trait IrClass extends IrSymbol {
  /** The class name represented by this symbol. */
  def className: String
  /** The primary constructor of the class. */
  def primaryConstructor: Option[IrConstructor]
  /** The methods defined on this class. (May be filtered to only those relevant to pickling interests. */
  def methods: Seq[IrMethod]
  /** True if this class is defined in Scala. */
  def isScala: Boolean
  /** True if this class is a scala case class. */
  def isCaseClass: Boolean
  /** True if this class is 'final' (or cannot be extended). */
  def isFinal: Boolean

  /** The set of known subclasses for this type.  Will return a failure if the symbol loader
    * isn't sure if the classes are closed.
    */
  def closedSubclasses: Try[Seq[IrClass]]
  /** Return the type of this class for a given universe. */
  def tpe[U <: Universe with Singleton](u: U): u.Type
}

sealed trait IrInnerClass extends IrSymbol with IrClass {
  def outerClass: IrClass
}
/** Represents a member of a particular class.
  * This might be a Constructor, Field or Member.
  */
sealed trait IrMember extends IrSymbol {
  /** The class that this member belongs to. */
  def owner: IrClass
  /** Returns true if the members is defined statically for the class. */
  def isStatic: Boolean
  /** Returns true if the given member is publicly accessible. */
  def isPublic: Boolean
  // TODO - Signatures

}
trait IrField extends IrMember {
  /** The name of the field, as we'd use to lookup via reflection. */
  def fieldName: String
}
trait IrMethod extends IrMember {
  /** The name of the method, as we'd use to lookup via reflection. */
  def methodName: String
  /** Returns true if this method is associated with a var. */
  def isVar: Boolean
  /** Returns the Scala type associated with this field. */
  def returnType[U <: Universe with Singleton](u: Universe): u.Type
}
/** The symbol representing a constructor. */
trait IrConstructor extends IrMember {
  /** The code-names of the constructor parameters.  Note: There is an algorithm which will try to
    * align the constructor parameters with getter methods by symbol name.
    *
    * We do this for case-classes.
    * @return
    */
  def parameterNames: Seq[String]
  /** Grabs the type of all the constructor parameters. */
  def parameterTypes[U <: Universe with Singleton](u: U): Seq[u.Type]
  def returnType[U <: Universe with Singleton](u: Universe): u.Type
}

import scala.reflect.api.Universe

/** A symbol loader for Java/Scala Symbols. */
abstract class IrSymbolLoader[U <: Universe with Singleton](val u: U) {
  /** Loads the symbols for a given Type using this symbol loader. */
  def newClass(tpe: u.Type): IrClass
}