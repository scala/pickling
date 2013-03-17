package scala

import java.lang.annotation.Inherited
import scala.annotation.MacroAnnotation
import scala.language.experimental.macros
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.universe._

package object pickling {

  // TOGGLE DEBUGGING
  var debugEnabled: Boolean = System.getProperty("pickling.debug", "false").toBoolean
  def debug(output: => String) = if (debugEnabled) println(output)

  implicit class PickleOps[T](picklee: T) {
    def pickle(implicit format: PickleFormat): _ = macro PickleMacros.pickle[T]
    def pickleInto(builder: PickleBuilder): _ = macro PickleMacros.pickleInto[T]
  }
}

package pickling {

  trait Pickler[T] {
    type PickleFormatType <: PickleFormat
    val format: PickleFormatType

    type PickleBuilderType <: PickleBuilder
    def pickle(picklee: Any, builder: PickleBuilderType): Unit
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

  trait Unpickler[T] {
    type PickleFormatType <: PickleFormat
    val format: PickleFormatType

    type PickleReaderType <: PickleReader
    def unpickle(tpe: Type, reader: PickleReaderType): Any
  }

  trait GenUnpicklers {
    implicit def genUnpickler[T](implicit format: PickleFormat): Unpickler[T] = macro UnpicklerMacros.impl[T]
    def genUnpickler(mirror: Mirror, tpe: Type)(implicit format: PickleFormat): Unpickler[_] = ??? // TODO: runtime dispatch for unpickling
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

    type PickleBuilderType <: PickleBuilder
    def createBuilder(): PickleBuilderType

    type PickleReaderType <: PickleReader
    def createReader(pickle: PickleType): PickleReaderType
  }

  trait PickleBuilder {
    type PickleType <: Pickle
    def beginEntry(tpe: Type, picklee: Any): this.type
    def putField(name: String, pickler: this.type => Unit): this.type
    def endEntry(): Unit
    def result(): PickleType
  }

  trait PickleReader {
    def readType(mirror: Mirror): Type
    def atPrimitive: Boolean
    def readPrimitive(tpe: Type): Any
    def atObject: Boolean
    def readField(name: String): this.type
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
