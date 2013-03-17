package scala.pickling

import scala.language.experimental.macros

import scala.reflect.api.Universe
import scala.reflect.runtime.{universe => ru}
import ir._

trait PickleFormat {

  type PickleType <: Pickle

  def instantiate = macro ???

  // TODO: unfortunately we hit a bug when trying to use the most specific signature possible, i.e. with the PickleType return value
  // when calling (pickleFormat: PickleFormat).formatRT(...), we compile finely, but then get an AME when trying to run the program:
  // java.lang.AbstractMethodError: scala.pickling.json.JSONPickleFormat.formatRT
  // (Lscala/pickling/ir/PickleIRs;Lscala/pickling/ir/PickleIRs$ClassIR;Ljava/lang/Object;Lscala/Function1;)Lscala/pickling/Pickle;
  // looks like scalac fails to generate a bridge here...
  // def formatRT[U <: Universe with Singleton](irs: PickleIRs[U])(cir: irs.ClassIR, picklee: Any, fields: irs.FieldIR => Pickle): PickleType
  def formatRT[U <: Universe with Singleton](irs: PickleIRs[U])(cir: irs.ClassIR, picklee: Any, fields: irs.FieldIR => Pickle): Pickle

  def getObject(p: PickleType): Any
  def getType(obj: Any, mirror: ru.Mirror): ru.Type
  def getField(obj: Any, name: String): Any
  def getPrimitive(obj: Any, tpe: ru.Type, name: String): Any

  /** Returns partial pickle */
  def putType(tpe: ru.Type): PickleType

  /** Adds field to `partial` pickle, using `state` to guide the pickling */
  def putField(partial: PickleType, state: Any, name: String, value: Any): PickleType // TODO: use ValueType for value

  /** Adds field of primitive type to `partial` pickle */
  def putPrimitive(partial: PickleType, state: Any, tpe: ru.Type, name: String, value: AnyVal): PickleType

  /** Adds object suffix, yields completed pickle */
  def putObjectSuffix(partial: PickleType, state: Any): PickleType

  def formatPrimitive(tpe: ru.Type, value: Any): PickleType
}
