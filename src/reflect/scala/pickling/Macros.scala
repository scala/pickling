package scala.pickling

import scala.reflect.macros.AnnotationMacro
import scala.reflect.macros.Macro
import scala.reflect.runtime.{universe => ru}
import ir._

trait PicklerMacros extends Macro {
  def impl[T: c.WeakTypeTag](pickleFormat: c.Expr[PickleFormat]): c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]

    // get instance of PickleFormat
    def failPickleFormat(msg: String) = c.abort(c.enclosingPosition, s"$msg for ${pickleFormat.tree} of type ${pickleFormat.tree.tpe}")
    val pickleFormatCarrier = c.typeCheck(q"$pickleFormat.instantiate", silent = true)
    val pickleFormatObj = pickleFormatCarrier.attachments.all.find(_.isInstanceOf[PickleFormat]) match {
      case Some(pf: PickleFormat) => pf
      case _ => failPickleFormat("Couldn't instantiate PickleFormat")
    }

    // get the type of pickles this format works with
    val pickleTypeCarrier = c.typeCheck(tq"${pickleFormat.tree.tpe}#PickleType", mode = c.TYPEmode, silent = true)
    val pickleType = pickleTypeCarrier match {
      case EmptyTree => failPickleFormat("Couldn't resolve PickleType")
      case tree => tree.tpe.normalize match {
        case tpe if tpe.typeSymbol.isClass => tpe
        case tpe => failPickleFormat(s"PickleType resolved as $tpe is invalid")
      }
    }

    // build pickler methods
    val irs = new PickleIRs[c.universe.type](c.universe)
    import irs._
    val cir = flatten(compose(ClassIR(tpe, null, List())))
    val nullCir = ClassIR(definitions.NullTpe, null, Nil)
    val picklee = Expr(q"picklee")
    val fieldAccessor = (fir: FieldIR) => Expr[Pickle](q"$picklee.${TermName(fir.name)}.pickle")
    def pickleLogic(cir: ClassIR) = pickleFormatObj.formatCT[c.universe.type](irs)(cir, picklee, fieldAccessor)

    q"""
      import scala.pickling._
      implicit val anon$$pickler = new Pickler[$tpe] {
        type PickleType = $pickleType
        def pickle(pickleeRaw: Any): $pickleType = {
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
  def impl[T: c.WeakTypeTag](pickler: c.Expr[Pickler[T]]) = {
    // TODO: generate the code like this
    // (new HasPicklerDispatch {
    //   def dispatchTo: Pickler[_] = p match {
    //     case null => genPickler[Person]
    //     case _: Person => genPickler[Person]
    //     case _: Employee => genPickler[Employee]
    //     case _: Any => runtimeFallback
    //   }
    // }).dispatchTo.pickle(p)
    // as described at https://github.com/heathermiller/pickling-design-doc/blob/gh-pages/index.md
    import c.universe._
    val q"${_}($picklee)" = c.prefix.tree
    Expr(q"$pickler.pickle($picklee)")
  }
}

trait UnpickleMacros extends Macro {
  def pickleUnpickle[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val pickleTree = c.prefix.tree

    // TODO: get rid of copy/paste w.r.t GenPicklerMacro
    def failUnpickle(msg: String) = c.abort(c.enclosingPosition, s"$msg for $pickleTree of type ${pickleTree.tpe}")
    val pickleFormatTypeCarrier = c.typeCheck(tq"${pickleTree.tpe}#PickleFormatType", mode = c.TYPEmode, silent = true)
    val pickleFormatType = pickleFormatTypeCarrier match {
      case EmptyTree => failUnpickle("Couldn't resolve PickleFormatType")
      case tree => tree.tpe.normalize match {
        case tpe if tpe.typeSymbol.isClass => tpe
        case tpe => failUnpickle(s"PickleFormatType resolved as $tpe is invalid")
      }
    }

    q"""
      new $pickleFormatType().parse($pickleTree, scala.reflect.runtime.currentMirror) match {
        case Some(result) => result.unpickle[$tpe]
        case None => throw new PicklingException("failed to unpickle \"" + $pickleTree + "\" as ${tpe.toString}")
      }
    """
  }
  def irUnpickle[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val irTree = c.prefix.tree

    def unpickleAs(tpe: Type) = q"scala.pickling.Unpickler.genUnpickler[$tpe].unpickle(ir)"
    val valueIRUnpickleLogic = unpickleAs(tpe)
    def allSubclasses(tpe: Type): List[Type] = List(tpe) // TODO: implement this and share the logic with Heather & Philipp
    val subclassDispatch = allSubclasses(tpe) map (tpe => CaseDef(Bind(TermName("tpe"), Ident(nme.WILDCARD)), q"tpe =:= scala.reflect.runtime.universe.typeOf[$tpe]", unpickleAs(tpe)))
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
