package scala.pickling

import scala.language.experimental.macros
import scala.language.existentials

import scala.reflect.macros.Context
import scala.reflect.runtime.{universe => ru}

// this is only necessary because 2.10.x doesn't support macro bundles
object Compat {
  // provides a source compatibility stub
  implicit class HasPt[A, B](t: (A, B)) {
    def pt: A = t._1
  }

  def PickleMacros_pickleTo[T: c.WeakTypeTag, S](c: Context)(output: c.Expr[S])(format: c.Expr[PickleFormat]): c.Expr[Unit] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PickleMacros
    c.Expr[Unit](bundle.pickleTo[T](output.tree)(format.tree))
  }
}
