package scala.pickling

import scala.reflect.macros.AnnotationMacro
import scala.reflect.runtime.{universe => ru}
import ir._

// purpose of this macro: implementation of genPickler[T]. i.e. the macro that is selected
// via implicit search and which initiates the process of generating a pickler for a given type T
trait PicklerMacros extends Macro {
  def impl[T: c.WeakTypeTag](pickleFormat: c.Tree): c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]

    import irs._
    val cir = flatten(compose(ClassIR(tpe, null, List())))
    val nullCir = ClassIR(definitions.NullTpe, null, Nil)
    val fieldAccessor = (fir: FieldIR) => Expr[Pickle](q"picklee.${TermName(fir.name)}.pickle")

    val rtag = c.reifyType(treeBuild.mkRuntimeUniverseRef, EmptyTree, tpe)

    def mkPrimitivePickler = q"""
      import scala.pickling._
      implicit val anon$$pickler = new Pickler[$tpe] {
        type PickleType = ${pickleType(pickleFormat)}
        def pickle(pickleeRaw: Any): PickleType = {
          val picklee = pickleeRaw.asInstanceOf[$tpe]
          $pickleFormat.formatPrimitive($rtag.tpe, picklee)
        }
      }
      anon$$pickler
    """

    if (tpe.typeSymbol.asClass.isPrimitive || tpe =:= typeOf[String])
      mkPrimitivePickler
    else {
      def pickleField(fir: irs.FieldIR): Tree =
        q"partial = pf.putField(partial, null, ${fir.name}, ${fieldAccessor(fir).tree}.value)"

      val pickleAllFieldsTree = cir.fields.map(pickleField).reduce((t1, t2) => q"""
        $t1
        $t2
      """)

      val pickleLogic = q"""
        val rtm = scala.reflect.runtime.universe.runtimeMirror(getClass.getClassLoader)
        val pf = $pickleFormat
        var partial = pf.putType($rtag.tpe)
        $pickleAllFieldsTree
        pf.putObjectSuffix(partial, null)
      """

      q"""
        import scala.pickling._
        implicit val anon$$pickler = new Pickler[$tpe] {
          type PickleType = ${pickleType(pickleFormat)}
          def pickle(pickleeRaw: Any): PickleType = {
            val picklee = pickleeRaw.asInstanceOf[$tpe]
            $pickleLogic
          }
        }
        anon$$pickler
      """
    }
  }
}

// purpose of this macro: implementation of genUnpickler[T]. i.e., the macro that is selected via implicit
// search and which initiates the process of generating an unpickler for a given type T.
trait UnpicklerMacros extends Macro {
  def impl[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    import definitions._
    import irs._

    val tpe = weakTypeOf[T]
    val expectsValueIR = tpe.typeSymbol.asClass.isPrimitive || tpe.typeSymbol == StringClass
    val expectsObjectIR = !expectsValueIR

    def genTreeToUnpickle(nestedObj: Name, nestedTpe: Type): Tree = {
      val cir     = flatten(compose(ClassIR(nestedTpe, null, List())))
      val ctorSym = nestedTpe.declaration(nme.CONSTRUCTOR).asMethod // TODO: multiple constructors

      def ctorArg(name: String, tpe: Type) = {
        tpe match { // TODO check if tpe is primitive, if so reify type
          case tpe if tpe =:= IntClass.toType    => q"pf.getPrimitive($nestedObj, ru.definitions.IntClass.toType, $name.toString).asInstanceOf[Int]"
          case tpe if tpe =:= StringClass.toType => q"pf.getPrimitive($nestedObj, ru.definitions.StringClass.toType, $name.toString).asInstanceOf[String]"
          case _ => // field has non-primitive type
            val newName = newTermName(c.freshName("picklNestedObj"))
            val treeToUnpickle = genTreeToUnpickle(newName, tpe)
            q"""
              {
                val $newName = pf.getField($nestedObj, $name)
                $treeToUnpickle
              }
            """
        }
      }
      val ctorArgs = ctorSym.paramss.flatten.map(f => ctorArg(f.name.toString, f.typeSignature)) // TODO: multiple argument lists
      q"new $nestedTpe(..$ctorArgs)"
    }

    val treeToUnpickle = genTreeToUnpickle(newTermName("obj"), tpe)

    q"""
     import scala.pickling._
     import scala.pickling.ir._
     import scala.reflect.runtime.{universe => ru}
     implicit val anon$$unpickler = new Unpickler[$tpe] {
       def unpickle(pickle: Pickle): $tpe = {
         val rtm = scala.reflect.runtime.universe.runtimeMirror(getClass.getClassLoader)
         val pf = implicitly[PickleFormat]
         val pickleTyped = pickle.asInstanceOf[pf.PickleType]
         val obj = pf.getObject(pickleTyped)
         $treeToUnpickle
       }
     }
     anon$$unpickler
    """
  }
}

// purpose of this macro: implementation of PickleOps.pickle. i.e., this exists so as to insert a call in the generated
// code to the genPickler macro (described above)
trait PickleMacros extends Macro {
  def impl[T: c.WeakTypeTag](pickleFormat: c.Tree) = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val q"${_}($picklee)" = c.prefix.tree

    def pickleAs(tpe: Type) = q"scala.pickling.Pickler.genPickler[$tpe]"
    val compileTimeDispatch = compileTimeDispatchees(tpe) map (tpe => {
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

// purpose of this macro: implementation of unpickle method on type Pickle.
trait UnpickleMacros extends Macro {
  // TODO: implement this
  // override def onInfer(tic: c.TypeInferenceContext): Unit = {
  //   c.error(c.enclosingPosition, "must specify the type parameter for method unpickle")
  // }
  def pickleUnpickle[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val pickleTree = c.prefix.tree

    def unpickleAs(tpe: Type) = q"scala.pickling.Unpickler.genUnpickler[$tpe].unpickle(pickle)"

    val compileTimeDispatch = compileTimeDispatchees(tpe) map (tpe => {
      CaseDef(Bind(TermName("tpe"), Ident(nme.WILDCARD)), q"tpe =:= scala.reflect.runtime.universe.typeOf[$tpe]", unpickleAs(tpe))
    })
    val runtimeDispatch = CaseDef(Ident(nme.WILDCARD), EmptyTree, q"???")

    val dispatchLogic = Match(q"pickleTpe", compileTimeDispatch :+ runtimeDispatch)

    q"""
      val pickle = $pickleTree
      val pf = new ${pickleFormatType(pickleTree)}()
      val pickleTpe = pf.getType(pf.getObject(pickle), scala.reflect.runtime.currentMirror)
      $dispatchLogic
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
