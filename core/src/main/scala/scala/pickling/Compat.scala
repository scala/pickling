package scala.pickling

import scala.language.experimental.macros
import scala.language.existentials

import scala.reflect.macros.Context
import scala.reflect.runtime.{universe => ru}
import pickler.ListPicklerUnpicklerMacro

// this is only necessary because 2.10.x doesn't support macro bundles
object Compat {
  // provides a source compatibility stub
  implicit class HasPt[A, B](t: (A, B)) {
    def pt: A = t._1
  }

  def PicklerMacros_impl[T: c.WeakTypeTag](c: Context): c.Expr[Pickler[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PicklerMacros
    c.Expr[Pickler[T]](bundle.impl[T])
  }

  def UnpicklerMacros_impl[T: c.WeakTypeTag](c: Context): c.Expr[Unpickler[T] with Generated] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with UnpicklerMacros
    c.Expr[Unpickler[T] with Generated](bundle.impl[T])
  }

  def PicklerUnpicklerMacros_impl[T: c.WeakTypeTag](c: Context): c.Expr[Pickler[T] with Unpickler[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PicklerUnpicklerMacros
    c.Expr[Pickler[T] with Unpickler[T]](bundle.impl[T])
  }

  def OpenSumUnpicklerMacro_impl[T: c.WeakTypeTag](c: Context): c.Expr[Unpickler[T] with Generated] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with OpenSumUnpicklerMacro
    c.Expr[Unpickler[T] with Generated](bundle.impl[T])
  }

  def PickleMacros_pickleTo[T: c.WeakTypeTag, S](c: Context)(output: c.Expr[S])(format: c.Expr[PickleFormat]): c.Expr[Unit] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PickleMacros
    c.Expr[Unit](bundle.pickleTo[T](output.tree)(format.tree))
  }

  def ListPicklerUnpicklerMacro_impl[T: c.WeakTypeTag](c: Context)(format: c.Expr[PickleFormat]): c.Expr[Pickler[T] with Unpickler[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with ListPicklerUnpicklerMacro
    c.Expr[Pickler[T] with Unpickler[T]](bundle.impl[T](format.tree))
  }

  def PicklerMacros_dpicklerImpl[T: c.WeakTypeTag](c: Context): c.Expr[DPickler[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PicklerMacros
    c.Expr[DPickler[T]](bundle.dpicklerImpl[T])
  }

  def CurrentMirrorMacro_impl(c: Context): c.Expr[ru.Mirror] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with CurrentMirrorMacro
    c.Expr[ru.Mirror](bundle.impl)
  }

  def FastTypeTagMacros_impl[T: c.WeakTypeTag](c: Context): c.Expr[FastTypeTag[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with FastTypeTagMacros
    c.Expr[FastTypeTag[T]](bundle.impl[T])
  }

  def FastTypeTagMacros_implClassTag[T: c.WeakTypeTag](c: Context): c.Expr[FastTypeTag[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with FastTypeTagMacros
    c.Expr[FastTypeTag[T]](bundle.implClassTag[T])
  }

  def FastTypeTagMacros_apply(c: Context)(key: c.Expr[String]): c.Expr[FastTypeTag[t]] forSome { type t } = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with FastTypeTagMacros
    c.Expr(bundle.apply(key.tree))
  }
}
