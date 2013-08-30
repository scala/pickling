package scala.pickling

import scala.reflect.macros.Context
import scala.reflect.api.{Universe => ApiUniverse}
import scala.reflect.runtime.{universe => ru}
import language.experimental.macros

// this is only necessary because 2.10.x doesn't support macro bundles
object Compat {
  def PicklerMacros_impl[T: c.WeakTypeTag](c: Context)(format: c.Expr[PickleFormat]): c.Expr[SPickler[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PicklerMacros
    c.Expr[SPickler[T]](bundle.impl[T](format.tree))
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

  def PickleMacros_pickleInto[T: c.WeakTypeTag](c: Context)(builder: c.Expr[PBuilder]): c.Expr[Unit] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PickleMacros
    c.Expr[Unit](bundle.pickleInto[T](builder.tree))
  }

  def PickleMacros_pickleTo[T: c.WeakTypeTag](c: Context)(output: c.Expr[Output[_]])(format: c.Expr[PickleFormat]): c.Expr[Unit] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with PickleMacros
    c.Expr[Unit](bundle.pickleTo[T](output.tree)(format.tree))
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

  def UnpickleMacros_readerUnpickleTopLevel[T: c.WeakTypeTag](c: Context): c.Expr[T] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with UnpickleMacros
    c.Expr[T](bundle.readerUnpickleTopLevel[T])
  }

  def ListPicklerUnpicklerMacro_impl[T: c.WeakTypeTag](c: Context)(format: c.Expr[PickleFormat]): c.Expr[SPickler[T] with Unpickler[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with ListPicklerUnpicklerMacro
    c.Expr[SPickler[T] with Unpickler[T]](bundle.impl[T](format.tree))
  }

  def VectorPicklerUnpicklerMacro_impl[T: c.WeakTypeTag](c: Context)(format: c.Expr[PickleFormat]): c.Expr[SPickler[T] with Unpickler[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with VectorPicklerUnpicklerMacro
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

  def FastTypeTagMacros_apply(c: Context)(key: c.Expr[String]): c.Expr[FastTypeTag[_]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with FastTypeTagMacros
    c.Expr[FastTypeTag[_]](bundle.apply(key.tree))
  }

  def ArrayBufferPicklerUnpicklerMacro_impl[T: c.WeakTypeTag](c: Context)(format: c.Expr[PickleFormat]): c.Expr[SPickler[T] with Unpickler[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with ArrayBufferPicklerUnpicklerMacro
    c.Expr[SPickler[T] with Unpickler[T]](bundle.impl[T](format.tree))
  }
}

trait QuasiquoteCompat { self: Macro =>
  val c: Context

  object quasiquoteCompat {
    val u: c.universe.type = c.universe
    import u._
    import Flag._

    // ================= LIFTABLES =================

    trait Liftable[T] {
      def apply(universe: ApiUniverse, value: T): universe.Tree
    }

    object Liftable {
      private class LiftableConstant[T] extends Liftable[T] {
        def apply(universe: ApiUniverse, value: T): universe.Tree =
          universe.Literal(universe.Constant(value))
      }

      implicit lazy val liftByte: Liftable[Byte] = new LiftableConstant[Byte]
      implicit lazy val liftShort: Liftable[Short] = new LiftableConstant[Short]
      implicit lazy val liftChar: Liftable[Char] = new LiftableConstant[Char]
      implicit lazy val liftInt: Liftable[Int] = new LiftableConstant[Int]
      implicit lazy val liftLong: Liftable[Long] = new LiftableConstant[Long]
      implicit lazy val liftFloat: Liftable[Float] = new LiftableConstant[Float]
      implicit lazy val liftDouble: Liftable[Double] = new LiftableConstant[Double]
      implicit lazy val liftBoolean: Liftable[Boolean] = new LiftableConstant[Boolean]
      implicit lazy val liftString: Liftable[String] = new LiftableConstant[String]
      implicit lazy val liftUnit: Liftable[Unit] = new LiftableConstant[Unit]

      // implicit lazy val liftScalaSymbol: Liftable[scala.Symbol] = new Liftable[scala.Symbol] {
      //   def apply(universe: ApiUniverse, value: scala.Symbol): universe.Tree = {
      //     import universe._
      //     q"scala.Symbol(${value.name})"
      //   }
      // }
    }

    private def requireSameUniverse[T](universe: ApiUniverse, tp: String, value: T) =
      require(universe eq u, s"Can't lift $tp ${showRaw(value)} from universe ${showRaw(universe)} using lift$tp defined for ${showRaw(u)}.")

    implicit def liftExpr[T <: Expr[_]]: Liftable[T] = new Liftable[T] {
      def apply(universe: ApiUniverse, value: T): universe.Tree = {
        requireSameUniverse(universe, "Expr", value)
        value.tree.asInstanceOf[universe.Tree]
      }
    }

    implicit object liftType extends Liftable[Type] {
      def apply(universe: ApiUniverse, value: Type): universe.Tree = {
        requireSameUniverse(universe, "Type", value)
        universe.TypeTree(value.asInstanceOf[universe.Type])
      }
    }

    implicit def liftTypeTag[T <: WeakTypeTag[_]]: Liftable[T] = new Liftable[T] {
      def apply(universe: ApiUniverse, value: T): universe.Tree = {
        requireSameUniverse(universe, "TypeTag", value)
        universe.TypeTree(value.asInstanceOf[universe.WeakTypeTag[_]].tpe)
      }
    }

    // ================= BUILD UTILS =================

    def Block(stats: List[Tree]): Block = stats match {
      case Nil => u.Block(Nil, Literal(Constant(())))
      case elem :: Nil => u.Block(Nil, elem)
      case elems => u.Block(elems.init, elems.last)
    }

    def annotationRepr(tree: Tree, args: List[Tree]): Tree = tree match {
      case ident: Ident => Apply(Select(New(ident), nme.CONSTRUCTOR: TermName), args)
      case call @ Apply(Select(New(ident: Ident), nme.CONSTRUCTOR), _) =>
        if(args.nonEmpty)
          throw new IllegalArgumentException("Can't splice annotation that already contains args with extra args.")
        call
      case _ => throw new IllegalArgumentException("Tree ${showRaw(tree)} isn't a correct representation of annotation.")
    }

    object FlagsAsBits {
      def unapply(flags: Long): Option[Long] = Some(flags)
    }

    object EmptyValDefLike {
      def unapply(t: Tree): Boolean = t eq emptyValDef
    }

    /** Applications in Scala can have one of the following shapes:
     *
     *    1) naked core: Ident(_) or Select(_, _) or basically anything else
     *    2) naked core with targs: TypeApply(core, targs) or AppliedTypeTree(core, targs)
     *    3) apply or several applies wrapping a core: Apply(core, _), or Apply(Apply(core, _), _), etc
     *
     *  This class provides different ways to decompose applications and simplifies their analysis.
     *
     *  ***Examples***
     *  (TypeApply in the examples can be replaced with AppliedTypeTree)
     *
     *    Ident(foo):
     *      * callee = Ident(foo)
     *      * core = Ident(foo)
     *      * targs = Nil
     *      * argss = Nil
     *
     *    TypeApply(foo, List(targ1, targ2...))
     *      * callee = TypeApply(foo, List(targ1, targ2...))
     *      * core = foo
     *      * targs = List(targ1, targ2...)
     *      * argss = Nil
     *
     *    Apply(foo, List(arg1, arg2...))
     *      * callee = foo
     *      * core = foo
     *      * targs = Nil
     *      * argss = List(List(arg1, arg2...))
     *
     *    Apply(Apply(foo, List(arg21, arg22, ...)), List(arg11, arg12...))
     *      * callee = foo
     *      * core = foo
     *      * targs = Nil
     *      * argss = List(List(arg11, arg12...), List(arg21, arg22, ...))
     *
     *    Apply(Apply(TypeApply(foo, List(targs1, targs2, ...)), List(arg21, arg22, ...)), List(arg11, arg12...))
     *      * callee = TypeApply(foo, List(targs1, targs2, ...))
     *      * core = foo
     *      * targs = Nil
     *      * argss = List(List(arg11, arg12...), List(arg21, arg22, ...))
     */
    class Applied(val tree: Tree) {
      /** The tree stripped of the possibly nested applications.
       *  The original tree if it's not an application.
       */
      def callee: Tree = {
        def loop(tree: Tree): Tree = tree match {
          case Apply(fn, _) => loop(fn)
          case tree         => tree
        }
        loop(tree)
      }

      /** The `callee` unwrapped from type applications.
       *  The original `callee` if it's not a type application.
       */
      def core: Tree = callee match {
        case TypeApply(fn, _)       => fn
        case AppliedTypeTree(fn, _) => fn
        case tree                   => tree
      }

      /** The type arguments of the `callee`.
       *  `Nil` if the `callee` is not a type application.
       */
      def targs: List[Tree] = callee match {
        case TypeApply(_, args)       => args
        case AppliedTypeTree(_, args) => args
        case _                        => Nil
      }

      /** (Possibly multiple lists of) value arguments of an application.
       *  `Nil` if the `callee` is not an application.
       */
      def argss: List[List[Tree]] = {
        def loop(tree: Tree): List[List[Tree]] = tree match {
          case Apply(fn, args) => loop(fn) :+ args
          case _               => Nil
        }
        loop(tree)
      }

      /** The depth of the nested applies: e.g. Apply(Apply(Apply(_, _), _), _)
       *  has depth 3.  Continues through type applications (without counting them.)
       */
      def applyDepth: Int = {
        def loop(tree: Tree): Int = tree match {
          case Apply(fn, _)           => 1 + loop(fn)
          case TypeApply(fn, _)       => loop(fn)
          case AppliedTypeTree(fn, _) => loop(fn)
          case _                      => 0
        }
        loop(tree)
      }
    }

    /** Returns a wrapper that knows how to destructure and analyze applications.
     */
    def dissectApplied(tree: Tree) = new Applied(tree)

    /** Destructures applications into important subparts described in `Applied` class,
     *  namely into: core, targs and argss (in the specified order).
     *
     *  Trees which are not applications are also accepted. Their callee and core will
     *  be equal to the input, while targs and argss will be Nil.
     *
     *  The provided extractors don't expose all the API of the `Applied` class.
     *  For advanced use, call `dissectApplied` explicitly and use its methods instead of pattern matching.
     */
    object Applied {
      def unapply(applied: Applied): Option[(Tree, List[Tree], List[List[Tree]])] =
        Some((applied.core, applied.targs, applied.argss))

      def unapply(tree: Tree): Option[(Tree, List[Tree], List[List[Tree]])] =
        unapply(dissectApplied(tree))
    }

    object Applied2 {
      def unapply(tree: Tree): Option[(Tree, List[List[Tree]])] = tree match {
        case Applied(fun, targs, argss) =>
          if(targs.length > 0)
            Some((TypeApply(fun, targs), argss))
          else
            Some((fun, argss))
        case _ => None
      }
    }

    def isEarlyDef(tree: Tree) = tree match {
      case TypeDef(mods, _, _, _) => mods hasFlag PRESUPER
      case ValDef(mods, _, _, _) => mods hasFlag PRESUPER
      case _ => false
    }

    def isEarlyValDef(tree: Tree) = tree match {
      case ValDef(mods, _, _, _) => mods hasFlag PRESUPER
      case _ => false
    }

    def isEarlyTypeDef(tree: Tree) = tree match {
      case TypeDef(mods, _, _, _) => mods hasFlag PRESUPER
      case _ => false
    }

    /** Is tree legal as a member definition of an interface?
     */
    def isInterfaceMember(tree: Tree): Boolean = tree match {
      case EmptyTree                     => true
      case Import(_, _)                  => true
      case TypeDef(_, _, _, _)           => true
      case DefDef(mods, _, _, _, _, __)  => mods hasFlag DEFERRED
      case ValDef(mods, _, _, _)         => mods hasFlag DEFERRED
      case _ => false
    }

    def mkSuperSelect = Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR)

    def copyValDef(tree: Tree)(
      mods: Modifiers = null,
      name: Name      = null,
      tpt: Tree       = null,
      rhs: Tree       = null
    ): ValDef = tree match {
      case ValDef(mods0, name0, tpt0, rhs0) =>
        treeCopy.ValDef(tree,
          if (mods eq null) mods0 else mods,
          if (name eq null) name0 else name,
          if (tpt eq null) tpt0 else tpt,
          if (rhs eq null) rhs0 else rhs
        )
      case t =>
        sys.error("Not a ValDef: " + t + "/" + t.getClass)
    }

    def ensureNonOverlapping(tree: Tree, others: List[Tree]){ ensureNonOverlapping(tree, others, true) }
    def ensureNonOverlapping(tree: Tree, others: List[Tree], focus: Boolean) {} // FIXME: what about -Yrangepos

    /** Generates a template with constructor corresponding to
     *
     *  constrmods (vparams1_) ... (vparams_n) preSuper { presupers }
     *  extends superclass(args_1) ... (args_n) with mixins { self => body }
     *
     *  This gets translated to
     *
     *  extends superclass with mixins { self =>
     *    presupers' // presupers without rhs
     *    vparamss   // abstract fields corresponding to value parameters
     *    def <init>(vparamss) {
     *      presupers
     *      super.<init>(args)
     *    }
     *    body
     *  }
     */
    def Template(parents: List[Tree], self: ValDef, constrMods: Modifiers, vparamss: List[List[ValDef]], argss: List[List[Tree]], body: List[Tree], superPos: Position): Template = {
      /* Add constructor to template */

      // create parameters for <init> as synthetic trees.
      var vparamss1 = vparamss map (_ map { vd =>
        atPos(vd.pos.focus) {
          val PARAMACCESSOR = scala.reflect.internal.Flags.PARAMACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]
          val flags1 = (vd.mods.flags.asInstanceOf[Long] & (IMPLICIT | DEFAULTPARAM | BYNAMEPARAM).asInstanceOf[Long]).asInstanceOf[FlagSet]
          val mods = u.Modifiers(flags1 | PARAM | PARAMACCESSOR)
          // FIXME: val mods1 = mods.withAnnotations(vd.mods.annotations)
          val mods1 = mods
          ValDef(mods1, vd.name, vd.tpt.duplicate, vd.rhs.duplicate)
        }
      })
      val (edefs, rest) = body span isEarlyDef
      val (evdefs, etdefs) = edefs partition isEarlyValDef
      val gvdefs = evdefs map {
        case vdef @ ValDef(_, _, tpt, _) =>
          copyValDef(vdef)(
          // atPos for the new tpt is necessary, since the original tpt might have no position
          // (when missing type annotation for ValDef for example), so even though setOriginal modifies the
          // position of TypeTree, it would still be NoPosition. That's what the author meant.
          // FIXME: tpt = atPos(vdef.pos.focus)(TypeTree() setOriginal tpt setPos tpt.pos.focus),
          tpt = atPos(vdef.pos.focus)(TypeTree()),
          rhs = EmptyTree
        )
      }
      // FIXME: val lvdefs = evdefs collect { case vdef: ValDef => copyValDef(vdef)(mods = vdef.mods | PRESUPER) }
      val lvdefs = evdefs collect { case vdef: ValDef => copyValDef(vdef)(mods = vdef.mods) }

      val constrs = {
        if (constrMods hasFlag TRAIT) {
          if (body forall isInterfaceMember) List()
          else List(
            atPos(wrappingPos(superPos, lvdefs)) (
              DefDef(NoMods, newTermName("$init$"), List(), List(List()), TypeTree(), u.Block(lvdefs, Literal(Constant())))))
        } else {
          // convert (implicit ... ) to ()(implicit ... ) if its the only parameter section
          if (vparamss1.isEmpty || !vparamss1.head.isEmpty && vparamss1.head.head.mods.hasFlag(IMPLICIT))
            vparamss1 = List() :: vparamss1;
          val superRef: Tree = atPos(superPos)(mkSuperSelect)
          val superCall = (superRef /: argss) (Apply.apply)
          List(
            atPos(wrappingPos(superPos, lvdefs ::: argss.flatten)) (
              DefDef(constrMods, nme.CONSTRUCTOR, List(), vparamss1, TypeTree(), u.Block(lvdefs ::: List(superCall), Literal(Constant())))))
        }
      }
      constrs foreach (ensureNonOverlapping(_, parents ::: gvdefs, focus=false))
      // Field definitions for the class - remove defaults.
      // FIXME: val fieldDefs = vparamss.flatten map (vd => copyValDef(vd)(mods = vd.mods &~ DEFAULTPARAM, rhs = EmptyTree))
      val fieldDefs = vparamss.flatten map (vd => copyValDef(vd)(mods = vd.mods, rhs = EmptyTree))

      u.Template(parents, self, gvdefs ::: fieldDefs ::: constrs ::: etdefs ::: rest)
    }

    object SyntacticClassDef {
      def apply(mods: Modifiers, name: TypeName, tparams: List[TypeDef],
                constrMods: Modifiers, vparamss: List[List[ValDef]], argss: List[List[Tree]],
                parents: List[Tree], selfdef: ValDef, body: List[Tree]): Tree =
        ClassDef(mods, name, tparams, Template(parents, selfdef, constrMods, vparamss, argss, body, NoPosition))

      def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef], Modifiers,
                                       List[List[ValDef]], List[List[Tree]], List[Tree], ValDef, List[Tree])] = tree match {
        case ClassDef(mods, name, tparams, Template(parents, selfdef, tbody)) =>
          // extract generated fieldDefs and constructor
          val (defs, (DefDef(mods, _, _, vparamss0, _, Block(_ :+ Applied(_, _, argss), _))) :: otherDefs) = tbody.splitAt(tbody.indexWhere {
            case DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => true
            case _ => false
          })
          val (earlyDefs, fieldDefs) = defs.span(isEarlyDef)

          // undo conversion from (implicit ... ) to ()(implicit ... ) when its the only parameter section
          val vparamss1 = vparamss0 match {
            case List() :: rest if !rest.isEmpty && !rest.head.isEmpty && rest.head.head.mods.hasFlag(IMPLICIT) => rest
            case other => other
          }

          // undo flag modifications by mergeing flag info from constructor args and fieldDefs
          val modsMap = fieldDefs.map { case ValDef(mods, name, _, _) => name -> mods }.toMap
          val vparamss2 = vparamss1.map { _.map { vd =>
            val mods1 = modsMap(vd.name)
            val flags1 = mods1.flags.asInstanceOf[Long]
            val flags2 = flags1 | (vd.mods.flags.asInstanceOf[Long] & DEFAULTPARAM.asInstanceOf[Long])
            val originalMods =
              if (flags1 == flags2) mods1
              else u.Modifiers(flags2.asInstanceOf[FlagSet], mods1.privateWithin, mods1.annotations) /* FIXME: setPositions positions */
            // val originalMods = modsMap(vd.name) | (vd.mods.flags & DEFAULTPARAM)
            atPos(vd.pos)(ValDef(originalMods, vd.name, vd.tpt, vd.rhs))
          }}

          Some((mods, name, tparams, mods, vparamss2, argss, parents, selfdef, earlyDefs ::: otherDefs))
        case _ =>
          None
      }
    }

    def True  = build.setType(Literal(Constant(true)), ConstantType(Constant(true)))
    def False = build.setType(Literal(Constant(false)), ConstantType(Constant(false)))

    object TermName {
      def apply(s: String) = newTermName(s)
      def unapply(name: TermName): Option[String] = Some(name.toString)
    }

    object TypeName {
      def apply(s: String) = newTypeName(s)
      def unapply(name: TypeName): Option[String] = Some(name.toString)
    }

    object Modifiers {
      def unapply(mods: Modifiers): Option[(FlagSet, Name, List[Tree])] = Some((mods.flags, mods.privateWithin, mods.annotations))
    }
  }
}

trait Reflection211Compat { self: Macro =>
  val c: Context
  import c.universe._

  object TermName {
    def apply(s: String) = newTermName(s)
    def unapply(name: TermName): Option[String] = Some(name.toString)
  }

  object TypeName {
    def apply(s: String) = newTypeName(s)
    def unapply(name: TypeName): Option[String] = Some(name.toString)
  }
}
