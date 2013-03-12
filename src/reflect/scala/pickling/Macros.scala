package scala.pickling

import scala.reflect.macros.AnnotationMacro
import scala.reflect.macros.Macro
import scala.reflect.runtime.{universe => ru}
import ir._

trait PicklerMacros extends Macro {
  def impl[T: c.WeakTypeTag](pickleFormat: c.Expr[PickleFormat]): c.Tree = {
    ???
  }
}

trait UnpicklerMacros extends Macro {
  def impl[T: c.WeakTypeTag]: c.Tree = {
    ???
  }
}

trait PickleMacros extends Macro {
  def impl[T: c.WeakTypeTag](pickler: c.Tree) = {
    ???
  }
}

trait UnpickleMacros extends Macro {
  def pickleUnpickle[T: c.WeakTypeTag]: c.Tree = {
    ???
  }
  def irUnpickle[T: c.WeakTypeTag]: c.Tree = {
    ???
  }
}

trait PickleableMacro extends AnnotationMacro {
  def impl = {
    ???
  }
}
