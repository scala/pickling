package scala.pickling
package generator

import scala.language.experimental.macros
import scala.language.existentials
import scala.pickling.Pickler

import scala.reflect.macros.Context
import scala.reflect.runtime.{universe => ru}

private[pickling] object Compat {

  def genPicklerUnpickler_impl[T: c.WeakTypeTag](c: Context): c.Expr[AbstractPicklerUnpickler[T] with Generated] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PicklingMacros
    c.Expr[AbstractPicklerUnpickler[T] with Generated](bundle.genPicklerUnpickler[T])
  }

}
