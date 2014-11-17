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

  def PicklerMacros_impl[T: c.WeakTypeTag](c: Context): c.Expr[SPickler[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PicklerMacros
    c.Expr[SPickler[T]](bundle.impl[T])
  }

  def UnpicklerMacros_impl[T: c.WeakTypeTag](c: Context): c.Expr[Unpickler[T] with Generated] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with UnpicklerMacros
    c.Expr[Unpickler[T] with Generated](bundle.impl[T])
  }

  def PickleMacros_pickle[T: c.WeakTypeTag](c: Context)(format: c.Expr[PickleFormat], tag: c.Expr[FastTypeTag[T]]): c.Expr[format.value.PickleType] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PickleMacros
    c.Expr[format.value.PickleType](bundle.pickle[T](format.tree))
  }

  def PickleMacros_pickleInto[T: c.WeakTypeTag](c: Context)(builder: c.Expr[PBuilder])(tag: c.Expr[FastTypeTag[T]]): c.Expr[Unit] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PickleMacros
    c.Expr[Unit](bundle.pickleInto[T](builder.tree))
  }

  def PickleMacros_pickleTo[T: c.WeakTypeTag](c: Context)(output: c.Expr[Output[_]])(format: c.Expr[PickleFormat], tag: c.Expr[FastTypeTag[T]]): c.Expr[Unit] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PickleMacros
    c.Expr[Unit](bundle.pickleTo[T](output.tree)(format.tree))
  }

  def UnpickleMacros_pickleUnpickle[T: c.WeakTypeTag](c: Context)(format: c.Expr[PickleFormat]): c.Expr[T] = {
    import c.universe._
    val c0: c.type = c
    val tpe = c.universe.weakTypeOf[T]
    // abort if someone forgets to pass a type parameter to the unpickle method
    val isNothing = tpe =:= definitions.NothingTpe
    val unpickleSym = c.mirror.staticClass("scala.pickling.UnpickleOps").asType.toType.member(newTermName("unpickle"))
    val typeArgMissing = tpe match {
      case t: TypeRef => t.typeSymbol.owner == unpickleSym || isNothing
      case _ => false
    }
    if (typeArgMissing)
      c.abort(c.enclosingPosition, """cannot unpickle because the (inferred) type argument of unpickle is abstract.
        |Typically, this is caused by omitting an explicit type argument. Always invoke unpickle with a concrete
        |type argument, for example, unpickle[Int]""".stripMargin)

    val tagTree = c.inferImplicitValue(appliedType(typeOf[FastTypeTag[_]].typeConstructor, List(tpe)))
    if (tagTree == EmptyTree)
      c.abort(c.enclosingPosition, s"could not find implicit FastTypeTag for type: $tpe")

    val bundle = new { val c: c0.type = c0 } with UnpickleMacros
    c.Expr[T](bundle.pickleUnpickle[T])
  }

  def ListPicklerUnpicklerMacro_impl[T: c.WeakTypeTag](c: Context)(format: c.Expr[PickleFormat]): c.Expr[SPickler[T] with Unpickler[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with ListPicklerUnpicklerMacro
    c.Expr[SPickler[T] with Unpickler[T]](bundle.impl[T](format.tree))
  }

  def PicklerMacros_dpicklerImpl[T: c.WeakTypeTag](c: Context)(format: c.Expr[PickleFormat]): c.Expr[DPickler[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PicklerMacros
    c.Expr[DPickler[T]](bundle.dpicklerImpl[T](format.tree))
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
