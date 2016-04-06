package scala.pickling
package tags

import scala.language.experimental.macros
import scala.reflect.macros.Context

// this is only necessary because 2.10.x doesn't support macro bundles
object Compat {
  def FastTypeTagMacros_impl[T: c.WeakTypeTag](c: Context): c.Expr[FastTypeTag[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with FastTypeTagMacros
    c.Expr[FastTypeTag[T]](bundle.impl[T])
  }
}
