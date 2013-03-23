package scala

import java.lang.annotation.Inherited
import scala.annotation.MacroAnnotation
import scala.language.experimental.macros
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.universe._
import scala.annotation.implicitNotFound

package object pickling {

  // TOGGLE DEBUGGING
  var debugEnabled: Boolean = System.getProperty("pickling.debug", "false").toBoolean
  def debug(output: => String) = if (debugEnabled) println(output)

  implicit class PickleOps[T](picklee: T) {
    def pickle(implicit format: PickleFormat): _ = macro PickleMacros.pickle[T]
    def pickleInto(builder: PickleBuilder): _ = macro PickleMacros.pickleInto[T]
  }

  def fastTypeTag[T]: TypeTag[T] = macro FastTypeTagMacro.impl[T]
}

package pickling {

  @implicitNotFound(msg = "Cannot generate a pickler for ${T}. Recompile with -Xlog-implicits for details")
  trait Pickler[T] {
    val format: PickleFormat
    def pickle(picklee: T, builder: PickleBuilder): Unit
  }

  trait GenPicklers {
    implicit def genPickler[T](implicit format: PickleFormat): Pickler[T] = macro PicklerMacros.impl[T]
    // TODO: the primitive pickler hack employed here is funny, but I think we should fix this one
    // since people probably would also have to deal with the necessity to abstract over pickle formats
    def genPickler(classLoader: ClassLoader, clazz: Class[_])(implicit format: PickleFormat, p1: Pickler[Int], p2: Pickler[String]): Pickler[_] = {
      println(s"generating runtime pickler for $clazz") // NOTE: needs to be an explicit println, so that we don't occasionally fallback to runtime in static cases
      val runtime = new CompiledPicklerRuntime(classLoader, clazz)
      // val runtime = new InterpretedPicklerRuntime(classLoader, clazz)
      runtime.genPickler
    }
  }

  object Pickler extends CorePicklersUnpicklers

  @implicitNotFound(msg = "Cannot generate an unpickler for ${T}. Recompile with -Xlog-implicits for details")
  trait Unpickler[T] {
    val format: PickleFormat
    def unpickle(tag: TypeTag[_], reader: PickleReader): Any
  }

  trait GenUnpicklers {
    implicit def genUnpickler[T](implicit format: PickleFormat): Unpickler[T] = macro UnpicklerMacros.impl[T]
    def genUnpickler(mirror: Mirror, tag: TypeTag[_])(implicit format: PickleFormat): Unpickler[_] = {
      println(s"generating runtime unpickler for ${tag.tpe}") // NOTE: needs to be an explicit println, so that we don't occasionally fallback to runtime in static cases
      val runtime = new CompiledUnpicklerRuntime(mirror, tag)
      // val runtime = new InterpretedUnpicklerRuntime(mirror, tag)
      runtime.genUnpickler
    }
  }

  object Unpickler extends CorePicklersUnpicklers

  trait Pickle {
    type ValueType
    val value: ValueType

    type PickleFormatType <: PickleFormat
    def unpickle[T] = macro UnpickleMacros.pickleUnpickle[T]
  }

  trait PickleFormat {
    type PickleType <: Pickle
    def createBuilder(): PickleBuilder
    def createReader(pickle: PickleType): PickleReader
  }

  trait PickleBuilder {
    def beginEntry(tag: TypeTag[_], picklee: Any, knownSize: Int = -1): this.type
    def beginEntryNoType(tag: TypeTag[_], picklee: Any, knownSize: Int = -1): this.type
    def putField(name: String, pickler: this.type => Unit): this.type
    def endEntry(): Unit
    def result(): Pickle
  }

  trait PickleReader {
    def readTag(mirror: Mirror): TypeTag[_]
    def atPrimitive: Boolean
    def readPrimitive(tag: TypeTag[_]): Any
    def atObject: Boolean
    def readField(name: String): PickleReader
    def unpickle[T] = macro UnpickleMacros.readerUnpickle[T]
  }

  @Inherited
  class pickleable extends MacroAnnotation {
    def transform = macro PickleableMacro.impl
  }

  // NOTE: can't call it Pickleable because of a name clash w.r.t pickleable on case-insensitive file systems
  trait PickleableBase {
    // TODO: what other methods do we want here?
  }

  case class PicklingException(msg: String) extends Exception(msg)
}
