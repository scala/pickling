package scala.pickling

import scala.reflect.macros.Context

// this is only necessary because 2.10.x doesn't support macro bundles
object Compat {
  def PicklerMacros_impl[T: c.WeakTypeTag](c: Context)(format: c.Expr[PickleFormat]): c.Expr[Pickler[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PicklerMacros
    c.Expr[Pickler[T]](bundle.impl[T](format.tree))
  }

  def UnpicklerMacros_impl[T: c.WeakTypeTag](c: Context)(format: c.Expr[PickleFormat]): c.Expr[Unpickler[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with UnpicklerMacros
    c.Expr[Unpickler[T]](bundle.impl[T](format.tree))
  }

  def PickleMacros_pickle[T: c.WeakTypeTag](c: Context)(format: c.Expr[PickleFormat]): c.Expr[Pickle] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PickleMacros
    c.Expr[Pickle](bundle.pickle[T](format.tree))
  }

  def PickleMacros_pickleInto[T: c.WeakTypeTag](c: Context)(builder: c.Expr[PickleBuilder]): c.Expr[Unit] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PickleMacros
    c.Expr[Unit](bundle.pickleInto[T](builder.tree))
  }

  def UnpickleMacros_pickleUnpickle[T: c.WeakTypeTag](c: Context): c.Expr[T] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with UnpickleMacros
    c.Expr[T](bundle.pickleUnpickle[T])
  }

  def UnpickleMacros_readerUnpickle[T: c.WeakTypeTag](c: Context): c.Expr[T] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with UnpickleMacros
    c.Expr[T](bundle.readerUnpickle[T])
  }

  def ListPicklerUnpicklerMacro_impl[T: c.WeakTypeTag](c: Context)(format: c.Expr[PickleFormat]): c.Expr[Pickler[T] with Unpickler[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with ListPicklerUnpicklerMacro
    c.Expr[Pickler[T] with Unpickler[T]](bundle.impl[T](format.tree))
  }
}
