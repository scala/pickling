package scala.pickling
package ir

import scala.reflect.runtime.universe._
import scala.collection.immutable.ListMap
import language.experimental.macros

sealed class UnpickleIR {
  def unpickle[T] = macro UnpickleMacros.irUnpickle[T]
}
case class ValueIR(value: Any) extends UnpickleIR
case class ObjectIR(tpe: Type, fields: ListMap[String, UnpickleIR]) extends UnpickleIR
