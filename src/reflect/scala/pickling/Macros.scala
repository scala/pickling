package scala.pickling

import scala.reflect.macros.AnnotationMacro
import scala.reflect.runtime.{universe => ru}
import ir._

trait PicklerMacros extends Macro {
  def impl[T: c.WeakTypeTag](pickleFormat: c.Tree): c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]

    import irs._
    val cir = flatten(compose(ClassIR(tpe, null, List())))
    val nullCir = ClassIR(definitions.NullTpe, null, Nil)
    val fieldAccessor = (fir: FieldIR) => Expr[Pickle](q"picklee.${TermName(fir.name)}.pickle")
    def pickleLogic(cir: ClassIR) = instantiatePickleFormat(pickleFormat).formatCT[c.universe.type](irs)(cir, Expr(q"picklee"), fieldAccessor)

    // TODO: genPickler and genUnpickler should really hoist their results to the top level
    // it should be a straightforward c.introduceTopLevel (I guess we can ignore inner classes in the paper)
    // then we would also not need this implicit val trick
    q"""
      import scala.pickling._
      implicit val anon$$pickler = new Pickler[$tpe] {
        type PickleType = ${pickleType(pickleFormat)}
        def pickle(pickleeRaw: Any): PickleType = {
          if (pickleeRaw != null) {
            val picklee = pickleeRaw.asInstanceOf[$tpe]
            ${pickleLogic(cir)}
          } else {
            ${pickleLogic(nullCir)}
          }
        }
      }
      anon$$pickler
    """
  }
}

trait UnpicklerMacros extends Macro {
  def impl[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    import definitions._
    val tpe = weakTypeOf[T]
    val expectsValueIR = tpe.typeSymbol.asClass.isPrimitive || tpe.typeSymbol == StringClass
    val expectsObjectIR = !expectsValueIR

    def unexpectedIR: c.Tree = q"""throw new PicklingException("unexpected IR: " + ir + " for type " + ${tpe.toString})"""

    def unpickleValueIR: c.Tree =
      tpe match {
        case tpe if tpe =:= IntClass.toType => q"vir.value.asInstanceOf[Double].toInt"
        case tpe if tpe =:= StringClass.toType => q"vir.value.asInstanceOf[String]"
        case _ => c.abort(c.enclosingPosition, s"don't know how to unpickle a ValueIR as $tpe")
      }

    def unpickleObjectIR: c.Tree = {
      val ctorSym = tpe.declaration(nme.CONSTRUCTOR).asMethod // TODO: multiple constructors
      def ctorArg(name: String, tpe: Type) = q"oir.fields($name).unpickle[$tpe]"
      val ctorArgs = ctorSym.paramss.flatten.map(f => ctorArg(f.name.toString, f.typeSignature)) // TODO: multiple argument lists
      q"new $tpe(..$ctorArgs)"
    }

    q"""
      import scala.pickling._
      import scala.pickling.ir._
      implicit val anon$$unpickler = new Unpickler[$tpe] {
        def unpickle(ir: UnpickleIR): $tpe = ir match {
          case vir: ValueIR => ${if (expectsValueIR) unpickleValueIR else unexpectedIR}
          case oir: ObjectIR => ${if (expectsObjectIR) unpickleObjectIR else unexpectedIR}
          case _ => $unexpectedIR
        }
      }
      anon$$unpickler
    """
  }
}

trait PickleMacros extends Macro {
  def impl[T: c.WeakTypeTag](pickleFormat: c.Tree) = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val q"${_}($picklee)" = c.prefix.tree

    def pickleAs(tpe: Type) = q"scala.pickling.Pickler.genPickler[$tpe]"
    val compileTimeDispatch = allStaticallyKnownSubclasses(tpe) map (tpe => {
      CaseDef(Bind(TermName("tpe"), Ident(nme.WILDCARD)), q"tpe =:= pickleeTpe", pickleAs(tpe))
    })
    val runtimeDispatch = CaseDef(Ident(nme.WILDCARD), EmptyTree, q"scala.pickling.Pickler.genPickler(mirror, pickleeTpe)")
    // TODO: this should go through HasPicklerDispatch
    // TODO: you know, probably we should move dispatch logic directly into picklers and unpicklers after some time
    // at the moment this won't help us at all, but later on when picklers/unpicklers will be hoisted, this will save us some bytecodes
    val dispatchLogic = Match(q"pickleeTpe", compileTimeDispatch :+ runtimeDispatch)

    // TODO: we don't really need ru.Types, neither here nor in unpickle dispatch
    // we could just use erasures and be perfectly fine. that would also bring a performance boost
    q"""
      import scala.pickling._
      val mirror = scala.reflect.runtime.currentMirror
      val pickleeTpe = mirror.reflect($picklee).symbol.asType.toType
      val pickler = $dispatchLogic.asInstanceOf[Pickler[_]{ type PickleType = ${pickleType(pickleFormat)} }]
      pickler.pickle($picklee)
    """
  }
}

trait UnpickleMacros extends Macro {
  def pickleUnpickle[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val pickleTree = c.prefix.tree

    q"""
      val pickle = $pickleTree
      new ${pickleFormatType(pickleTree)}().parse(pickle, scala.reflect.runtime.currentMirror) match {
        case Some(result) => result.unpickle[$tpe]
        case None => throw new PicklingException("failed to unpickle \"" + pickle + "\" as ${tpe.toString}")
      }
    """
  }
  def irUnpickle[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val irTree = c.prefix.tree

    def unpickleAs(tpe: Type) = q"scala.pickling.Unpickler.genUnpickler[$tpe].unpickle(ir)"
    val valueIRUnpickleLogic = unpickleAs(tpe)
    val subclassDispatch = allStaticallyKnownSubclasses(tpe) map (tpe => {
      CaseDef(Bind(TermName("tpe"), Ident(nme.WILDCARD)), q"tpe =:= scala.reflect.runtime.universe.typeOf[$tpe]", unpickleAs(tpe))
    })
    val runtimeDispatch = CaseDef(Ident(nme.WILDCARD), EmptyTree, q"???")
    // TODO: this should also go through HasPicklerDispatch, probably routed through a companion of T
    val objectIRUnpickleLogic = Match(q"ir.tpe", subclassDispatch :+ runtimeDispatch)

    q"""
      import scala.pickling._
      import scala.pickling.ir._
      val ir = $irTree
      ir match {
        case ir: ValueIR => $valueIRUnpickleLogic
        case ir: ObjectIR => $objectIRUnpickleLogic
        case ir => throw new PicklingException(s"unknown IR: $$ir")
      }
    """
  }
}

trait PickleableMacro extends AnnotationMacro {
  def impl = {
    import c.universe._
    import Flag._
    c.annottee match {
      case ClassDef(mods, name, tparams, Template(parents, self, body)) =>
        // TODO: implement dispatchTo, add other stuff @pickleable should do
        val dispatchTo = q"override def dispatchTo = ???"
        ClassDef(mods, name, tparams, Template(parents :+ tq"scala.pickling.HasPicklerDispatch", self, body :+ dispatchTo))
    }
  }
}
