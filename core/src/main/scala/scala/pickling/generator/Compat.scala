package scala.pickling
package generator

import scala.language.experimental.macros
import scala.language.existentials
import scala.pickling.Pickler

import scala.reflect.macros.Context
import scala.reflect.runtime.{universe => ru}

private[pickling] object Compat {
  def genPickler_impl[T: c.WeakTypeTag](c: Context): c.Expr[Pickler[T] with Generated] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PicklingMacros
    c.Expr[Pickler[T] with Generated](bundle.genPickler[T])
  }

  def genPicklerUnpickler_impl[T: c.WeakTypeTag](c: Context): c.Expr[AbstractPicklerUnpickler[T] with Generated] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PicklingMacros
    c.Expr[AbstractPicklerUnpickler[T] with Generated](bundle.genPicklerUnpickler[T])
  }

  def genUnpickler_impl[T: c.WeakTypeTag](c: Context): c.Expr[Unpickler[T] with Generated] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PicklingMacros
    c.Expr[Unpickler[T] with Generated](bundle.genUnPickler[T])
  }

  def genPicklerUnpickler_debug[T: c.WeakTypeTag](c: Context): c.Expr[String] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PicklingMacros
    c.Expr[String](bundle.debugPicklerUnpickler[T])
  }
}
