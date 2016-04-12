package scala.pickling
package tags

import scala.language.experimental.macros

/** Macros which take compiler types and turn them into
 *  runtime string we can use to tag ADTs.
 *  Additionally, these strings SHOULD be 100% compatible
 *  with type tags generated from raw java reflection.
 */
trait FastTypeTagMacros extends Macro {
  // TODO(joshuasuereth): This is may duplicate functionality with the `.tag` extension method on `Type`.
  def impl[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    import compat._
    val T = weakTypeOf[T]
    if (T.typeSymbol.isParameter)
      c.abort(c.enclosingPosition, s"cannot generate FastTypeTag for type parameter $T, FastTypeTag can only be generated for concrete types")
    def handleType(t: c.Type): c.Tree =
      t.normalize match {
        case ExistentialType(tparams, TypeRef(pre, sym, targs))
          if targs.nonEmpty && targs.forall(targ => tparams.contains(targ.typeSymbol)) =>
            // rather than going down form List[_] => List we want to become List[Any],
            // we are trying to make this match the java-reflection case (where it will be Any)
            handleType(TypeRef(pre, sym, targs.map(_ => definitions.AnyTpe)))
        // Note: This is incompatible with 2.10.x, where AnnotatedType takes 3 args
        case AnnotatedType(_, raw, _) => handleType(raw)
        case RefinedType(first :: tail, scope) => handleType(first)
        // TODO - Special handling for inner-classes of classes
        case TypeRef(pre, sym, targs) if pre.typeSymbol.isModuleClass =>
	        val name = sym.fullName + (if (sym.isModuleClass) ".type" else "")
	        val targSrcs = targs.map(t => q"_root_.scala.Predef.implicitly[_root_.scala.pickling.tags.FastTypeTag[${t}]]")
	        q"_root_.scala.pickling.tags.FastTypeTag[$T]($name, _root_.scala.List.apply(..$targSrcs))"
      case _ =>
	        q"_root_.scala.pickling.tags.FastTypeTag[$T](${T.toString}, _root_.scala.Nil)"
    }
    handleType(T)
  }
}