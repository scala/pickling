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
    def pickle(implicit pickleFormat: PickleFormat): _ = macro PickleMacros.impl[T]
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
      println(s"generating runtime pickler for $tpe") // TODO: needs to be an explicit println, so that we don't occasionally fallback to runtime in static cases
      PicklerRuntime.genCompiledPickler(mirror, tpe)
      // PicklerRuntime.genInterpretedPickler(mirror, tpe)
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
    // TODO: unfortunately we hit a bug when trying to use the most specific signature possible, i.e. with the PickleType return value
    // when calling (pickleFormat: PickleFormat).formatRT(...), we compile finely, but then get an AME when trying to run the program:
    // java.lang.AbstractMethodError: scala.pickling.json.JSONPickleFormat.formatRT
    // (Lscala/pickling/ir/PickleIRs;Lscala/pickling/ir/PickleIRs$ClassIR;Ljava/lang/Object;Lscala/Function1;)Lscala/pickling/Pickle;
    // looks like scalac fails to generate a bridge here...
    // def formatRT[U <: Universe with Singleton](irs: PickleIRs[U])(cir: irs.ClassIR, picklee: Any, fields: irs.FieldIR => Pickle): PickleType
    def formatRT[U <: Universe with Singleton](irs: PickleIRs[U])(cir: irs.ClassIR, picklee: Any, fields: irs.FieldIR => Pickle): Pickle
    def parse(pickle: PickleType, mirror: ru.Mirror): Option[UnpickleIR]
  }

  case class PicklingException(msg: String) extends Exception(msg)
}
