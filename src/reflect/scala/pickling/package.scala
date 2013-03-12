package scala

import java.lang.annotation.Inherited
import scala.annotation.MacroAnnotation
import scala.language.experimental.macros
import scala.reflect.runtime.{universe => ru}
import scala.reflect.api.Universe

package object pickling {
  // TOGGLE DEBUGGING
  var debugEnabled: Boolean = System.getProperty("pickling.debug", "false").toBoolean
  def debug(output: => String) = if (debugEnabled) println(output)

  implicit class PickleOps[T](picklee: T) {
    def pickle: _ = macro PickleMacros.impl[T]
  }
}

package pickling {

  trait Pickler[T] {
    type PickleType <: Pickle
    def pickle(picklee: Any): PickleType
  }

  object Pickler {
    implicit def genPickler[T](implicit pickleFormat: PickleFormat): Pickler[T] = macro PicklerMacros.impl[T]
    // TODO: the primitive pickler hack employed here is funny, but I think we should fix this one
    // since people probably would also have to deal with the necessity to abstract over pickle formats
    def genPickler(mirror: ru.Mirror, tpe: ru.Type)(implicit format: PickleFormat, p1: Pickler[Int], p2: Pickler[String]): Pickler[_] = {
      // PicklerRuntime.genCompiledPickler(mirror, tpe)
      PicklerRuntime.genInterpretedPickler(mirror, tpe)
    }
  }

  trait Unpickler[T] {
    import ir._
    def unpickle(ir: UnpickleIR): T
  }

  object Unpickler {
    implicit def genUnpickler[T]: Unpickler[T] = macro UnpicklerMacros.impl[T]
  }

  trait Pickle {
    type ValueType
    val value: ValueType

    type PickleFormatType <: PickleFormat
    def unpickle[T] = macro UnpickleMacros.pickleUnpickle[T]
  }

  @Inherited
  class pickleable extends MacroAnnotation {
    def transform = macro PickleableMacro.impl
  }

  trait HasPicklerDispatch {
    def dispatchTo: Pickler[_]
  }

  trait PickleFormat {
    import ir._
    type PickleType <: Pickle
    def instantiate = macro ???
    def formatCT[U <: Universe with Singleton](irs: PickleIRs[U])(cir: irs.ClassIR, picklee: U#Expr[Any], fields: irs.FieldIR => U#Expr[Pickle]): U#Expr[PickleType]
    def formatRT[U <: Universe with Singleton](irs: PickleIRs[U])(cir: irs.ClassIR, picklee: Any, fields: irs.FieldIR => Pickle): PickleType
    def parse(pickle: PickleType, mirror: ru.Mirror): Option[UnpickleIR]
  }

  case class PicklingException(msg: String) extends Exception(msg)
}
