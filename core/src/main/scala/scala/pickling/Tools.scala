package scala.pickling

import scala.pickling.internal._

import scala.language.existentials

import scala.reflect.macros.Context
import scala.reflect.api.Universe

import scala.collection.mutable.{Map => MutableMap, ListBuffer => MutableList, WeakHashMap, Set => MutableSet}
import scala.collection.mutable.{Stack => MutableStack, Queue => MutableQueue}

import java.lang.ref.WeakReference

import HasCompat._

object Tools {
  private val subclassCaches = new WeakHashMap[AnyRef, WeakReference[AnyRef]]()

  private object SomeRef {
    def unapply[T](optRef: Option[WeakReference[T]]): Option[T] =
      if (optRef.nonEmpty) {
        val result = optRef.get.get
        if (result != null) Some(result) else None
      } else None
  }

  def subclassCache(key: AnyRef, valueThunk: => AnyRef): AnyRef = {
    subclassCaches get key match {
      case SomeRef(value) =>
        value
      case _ =>
        val value = valueThunk
        subclassCaches(key) = new WeakReference(value)
        value
    }
  }
}

class Tools[C <: Context](val c: C) {
  val u: c.universe.type = c.universe
  import u._
  import compat._
  import definitions._

  private def directSubclassesAnnotation(sym: TypeSymbol): Option[Seq[TypeSymbol]] = {
    val annotatedSubclasses = sym.annotations.collect({
      case annotation if annotation.tpe == typeOf[scala.pickling.directSubclasses] =>
        annotation
    }).headOption map {
      annotation =>
        annotation.javaArgs(newTermName("value")) match {
          case ArrayArgument(klasses) => klasses.toList map {
            case LiteralArgument(constant) =>
              constant.value.asInstanceOf[Type].typeSymbol.asType
          }
        }
    }

    annotatedSubclasses
  }

  /** Find direct subclasses, preferring the directSubclasses annotation
   * over knownDirectSubclasses.
   */
  def directSubclasses(sym: ClassSymbol): Seq[Symbol] = {
    directSubclassesAnnotation(sym).getOrElse(sym.knownDirectSubclasses.toList)
  }

  /** Treat as a sealed type because it either is sealed, or specifies
   * directSubclasses annotation.
   */
  def treatAsSealed(sym: ClassSymbol): Boolean =
    sym.isSealed || directSubclassesAnnotation(sym).isDefined

  def blackList(sym: Symbol) = sym == AnyClass || sym == AnyRefClass || sym == AnyValClass || sym == ObjectClass

  def isRelevantSubclass(baseSym: Symbol, subSym: Symbol) = {
    !blackList(baseSym) && !blackList(subSym) && subSym.isClass && {
      val subClass = subSym.asClass
      subClass.baseClasses.contains(baseSym) && !subClass.isAbstractClass && !subClass.isTrait
    }
  }

  def compileTimeDispatchees(tpe: Type, mirror: Mirror, excludeSelf: Boolean): List[Type] = {
    val subtypes = allStaticallyKnownConcreteSubclasses(tpe, mirror).filter(subtpe => subtpe.typeSymbol != tpe.typeSymbol)
    val selfTpe = if (isRelevantSubclass(tpe.typeSymbol, tpe.typeSymbol)) List(tpe) else Nil
    val result = if (excludeSelf) subtypes else subtypes ++ selfTpe
    // println(s"$tpe => $result")
    result
  }

  def allStaticallyKnownConcreteSubclasses(tpe: Type, mirror: Mirror): List[Type] = {
    // TODO: so far the search is a bit dumb
    // given `class C[T]; class D extends C[Int]` and `tpe = C[String]`, it will return <symbol of D>
    // TODO: on a more elaborate note
    // given `class C; class D[T] extends C` we of course cannot return the infinite number of `D[X]` types
    // but what we can probably do is to additionally look up custom picklers/unpicklers of for specific `D[X]`
    val baseSym = tpe.typeSymbol.asType
    val baseTargs = tpe match { case TypeRef(_, _, args) => args; case _ => Nil }

    def sourcepathScan(): List[Symbol] = {
      val subclasses = MutableList[Symbol]()
      def analyze(sym: Symbol) = if (isRelevantSubclass(baseSym, sym)) subclasses += sym
      def loop(tree: Tree): Unit = tree match {
        // NOTE: only looking for classes defined in objects or top-level classes!
        case PackageDef(_, stats) => stats.foreach(loop)
        case cdef: ClassDef => analyze(cdef.symbol)
        case mdef: ModuleDef => mdef.impl.body.foreach(loop)
        case _ => // do nothing
      }
      c.enclosingRun.units.map(_.body).foreach(loop)
      subclasses.toList
    }

    def sealedHierarchyScan(): List[Symbol] = {
      var hierarchyIsSealed = true
      def loop(sym: ClassSymbol): List[ClassSymbol] = {
        sym +: {
          val initialize = sym.typeSignature
          if (sym.isFinal || sym.isModuleClass) {
            Nil
          } else if (treatAsSealed(sym)) {
            val syms: List[ClassSymbol] =
              directSubclasses(sym).map {
                case csym: ClassSymbol => csym
                case msym: ModuleSymbol => msym.moduleClass.asClass
                case osym => throw new Exception(s"unexpected known direct subclass: $osym <: $sym")
              }.toList.flatMap(loop)
            syms
          } else {
            hierarchyIsSealed = false
            Nil
          }
        }
      }
      if (baseSym.isClass) {
        val sealedHierarchy = loop(baseSym.asClass)
        if (hierarchyIsSealed) sealedHierarchy
        else sealedHierarchy ++ sourcepathScan() //sourcepathAndClasspathScan()
      } else sourcepathScan() //sourcepathAndClasspathScan()
    }

    def sourcepathAndClasspathScan(): List[Symbol] = {
      println(s"full classpath scan: $tpe")
      lazy val classpathCache = Tools.subclassCache(mirror, {
        val cache = MutableMap[Symbol, MutableList[Symbol]]()
        def updateCache(bc: Symbol, c: Symbol) = {
          if (bc != c && isRelevantSubclass(bc, c)) // TODO: what else do we want to ignore?
            cache.getOrElseUpdate(bc, MutableList()) += c
        }
        def loop(pkg: Symbol): Unit = {
          // NOTE: only looking for top-level classes!
          val pkgMembers = pkg.typeSignature.members
          pkgMembers foreach (m => {
            def analyze(m: Symbol): Unit = {
              if (m.name.decoded.contains("$")) () // SI-7251
              else if (m.isClass) m.asClass.baseClasses foreach (bc => updateCache(bc, m))
              else if (m.isModule) analyze(m.asModule.moduleClass)
              else ()
            }
            analyze(m)
          })
          def recurIntoPackage(pkg: Symbol) = {
            pkg.name.toString != "_root_" &&
            pkg.name.toString != "quicktime" && // TODO: pesky thing on my classpath, crashes ClassfileParser
            pkg.name.toString != "j3d" && // TODO: another ClassfileParser crash
            pkg.name.toString != "jansi" && // TODO: and another one (jline.jar)
            pkg.name.toString != "jsoup" // TODO: SI-3809
          }
          val subpackages = pkgMembers filter (m => m.isPackage && recurIntoPackage(m))
          subpackages foreach loop
        }
        loop(mirror.RootClass)
        cache // NOTE: 126873 cache entries for my classpath
      }).asInstanceOf[MutableMap[Symbol, MutableList[Symbol]]]
      classpathCache.getOrElse(baseSym, Nil).toList
    }

    if (baseSym.isFinal || baseSym.isModuleClass) Nil // FIXME: http://groups.google.com/group/scala-internals/browse_thread/thread/e2b786120b6d118d
    else if (blackList(baseSym)) Nil
    else {
      var unsorted = {
        if (baseSym.isClass && treatAsSealed(baseSym.asClass)) sealedHierarchyScan()
        else sourcepathScan() // sourcepathAndClasspathScan()
      }
      // NOTE: need to order the list: children first, parents last
      // otherwise pattern match which uses this list might work funnily
      val subSyms = unsorted.distinct.sortWith((c1, c2) => c1.asClass.baseClasses.contains(c2))
      val subTpes = subSyms.map(_.asClass).map(subSym => {
        def tparamNames(sym: TypeSymbol) = sym.typeParams.map(_.name.toString)
        // val tparamsMatch = subSym.typeParams.nonEmpty && tparamNames(baseSym) == tparamNames(subSym)
        val tparamsMatch = subSym.typeParams.nonEmpty && tparamNames(baseSym).length == tparamNames(subSym).length
        val targsAreConcrete = baseTargs.nonEmpty && baseTargs.forall(_.typeSymbol.isClass)
        // NOTE: this is an extremely naïve heuristics
        // see http://groups.google.com/group/scala-internals/browse_thread/thread/3a43a6364b97b521 for more information
        if (tparamsMatch && targsAreConcrete) appliedType(subSym.toTypeConstructor, baseTargs)
        else existentialAbstraction(subSym.typeParams, subSym.toType)
      })
      subTpes
    }
  }
}

trait RichTypes {
  type MyUniverse <: Universe
  val u: MyUniverse

  import u._
  import definitions._
  import compat._

  implicit class RichType(tpe: scala.reflect.api.Universe#Type) {

    def isEffectivelyPrimitive: Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && eltpe.typeSymbol.isClass && eltpe.typeSymbol.asClass.isPrimitive => true
      case _ => false
    }

    def isEffectivelyFinal = tpe.typeSymbol.isEffectivelyFinal

    def isNotNullable = tpe.typeSymbol.isNotNullable
  }
}

abstract class ShareAnalyzer[U <: Universe](val u: U) extends RichTypes {
  type MyUniverse = U

  import u._
  import definitions._

  val irs = new ir.IRs[u.type](u)
  import irs._

  def shareEverything: Boolean
  def shareNothing: Boolean

  // TODO: cache this, because it's not cheap and it's going to be called a lot of times for the same types
  def canCauseLoops(tpe: Type): Boolean = {
    def loop(todo: List[Type], visited: Set[Type]): Boolean = {
      todo match {
        case currTpe :: rest =>
          val currSym = currTpe.typeSymbol.asType
          if (visited(currTpe)) {
            if (tpe <:< currTpe) true  // TODO: make sure this sanely works for polymorphic types
            else loop(rest, visited)
          } else if (currTpe.isNotNullable || currTpe.isEffectivelyPrimitive || currSym == StringClass || currSym.isModuleClass) loop(rest, visited)
          // TODO: extend the traversal logic to support sealed classes
          // when doing that don't forget:
          // 1) sealeds can themselves be extended, so we need to recur
          // 2) the entire sealed hierarchy should be added to todo
          else if (!currSym.isFinal) true // NOTE: returning true here is important for soundness!
          else {
            val more = newClassIR(currTpe).fields.map(_.tpe)
            loop(rest ++ more, visited + currTpe)
          }
        case _ => false
      }
    }
    loop(List(tpe), Set())
  }

  def shouldBotherAboutSharing(tpe: Type): Boolean = {
    if (shareNothing) false
    else if (shareEverything) !tpe.isEffectivelyPrimitive || (tpe.typeSymbol.asType == StringClass)
    else canCauseLoops(tpe)
  }

  def shouldBotherAboutLooping(tpe: Type): Boolean = {
    if (shareNothing) false
    else canCauseLoops(tpe)
  }

  def shouldBotherAboutCleaning(tpe: Type): Boolean = {
    if (shareNothing) false
    else true // TODO: need to be more precise here
  }
}

abstract class Macro extends RichTypes { self =>
  type MyUniverse = scala.reflect.macros.Universe

  val c: Context
  val u: MyUniverse = c.universe

  import c.universe._
  import compat._
  import definitions._

  val RefTpe = weakTypeOf[refs.Ref]

  val tools = new Tools[c.type](c)
  import tools._

  val shareAnalyzer = new ShareAnalyzer[c.universe.type](c.universe) {
    def shareEverything = self.shareEverything
    def shareNothing = self.shareNothing
  }

  val irs = new ir.IRs[c.universe.type](c.universe)
  import irs._

  def shouldBotherAboutCleaning(tpe: Type) = shareAnalyzer.shouldBotherAboutCleaning(tpe)
  def shouldBotherAboutSharing(tpe: Type) = shareAnalyzer.shouldBotherAboutSharing(tpe)
  def shouldBotherAboutLooping(tpe: Type) = shareAnalyzer.shouldBotherAboutLooping(tpe)

  def shareEverything = {
    val shareEverything = c.inferImplicitValue(typeOf[refs.ShareEverything]) != EmptyTree
    val shareNothing = c.inferImplicitValue(typeOf[refs.ShareNothing]) != EmptyTree
    if (shareEverything && shareNothing) c.abort(c.enclosingPosition, "inconsistent sharing configuration: both ShareEverything and ShareNothing are in scope")
    shareEverything
  }

  def shareNothing = {
    val shareEverything = c.inferImplicitValue(typeOf[refs.ShareEverything]) != EmptyTree
    val shareNothing = c.inferImplicitValue(typeOf[refs.ShareNothing]) != EmptyTree
    if (shareEverything && shareNothing) c.abort(c.enclosingPosition, "inconsistent sharing configuration: both ShareEverything and ShareNothing are in scope")
    shareNothing
  }

  def compileTimeDispatcheesNotSelf(tpe: Type): List[Type] = tools.compileTimeDispatchees(tpe, rootMirror, true)

  def compileTimeDispatchees(tpe: Type): List[Type] = tools.compileTimeDispatchees(tpe, rootMirror, false)

  def compileTimeDispatcheesNotEmpty(tpe: Type): List[Type] = {
    val dispatchees = compileTimeDispatchees(tpe)
    // this will catch at compile time a total failure of
    // knownDirectSubclasses to find subtypes, though it won't
    // catch a partial failure of knownDirectSubclasses
    if (dispatchees.isEmpty)
      throw new Exception(s"Didn't find any concrete subtypes of abstract $tpe, this may mean you need to use the @directSubclasses annotation to manually tell the compiler about subtypes")
    dispatchees
  }

  def syntheticPackageName: String = "scala.pickling.synthetic"
  def syntheticBaseName(tpe: Type): TypeName = {
    val raw = tpe.toString.split('.').map(_.capitalize).mkString("")
    val encoded = newTypeName(raw).encoded
    newTypeName(encoded)
  }
  def syntheticBaseQualifiedName(tpe: Type): TypeName = newTypeName(syntheticPackageName + "." + syntheticBaseName(tpe).toString)

  def syntheticPicklerName(tpe: Type): TypeName = syntheticBaseName(tpe) + syntheticPicklerSuffix()
  def syntheticPicklerQualifiedName(tpe: Type): TypeName = syntheticBaseQualifiedName(tpe) + syntheticPicklerSuffix()
  def syntheticPicklerSuffix(): String = "Pickler"

  def syntheticUnpicklerName(tpe: Type): TypeName = syntheticBaseName(tpe) + syntheticUnpicklerSuffix()
  def syntheticUnpicklerQualifiedName(tpe: Type): TypeName = syntheticBaseQualifiedName(tpe) + syntheticUnpicklerSuffix()
  def syntheticUnpicklerSuffix(): String = "Unpickler"

  def syntheticPicklerUnpicklerName(tpe: Type): TypeName = syntheticBaseName(tpe) + syntheticPicklerUnpicklerSuffix()
  def syntheticPicklerUnpicklerQualifiedName(tpe: Type): TypeName = syntheticBaseQualifiedName(tpe) + syntheticPicklerUnpicklerSuffix()
  def syntheticPicklerUnpicklerSuffix(): String = "PicklerUnpickler"

  def preferringAlternativeImplicits(body: => Tree): Tree = {
    import Compat._

    val candidates = c.enclosingImplicits
    if (candidates.isEmpty)
      return body
    val ourPt      = candidates.head.pt

    def debug(msg: Any) = {
      val padding = "  " * (candidates.length - 1)
      // Console.err.println(padding + msg)
    }

    debug("can we enter " + ourPt + "?")
    debug(candidates)

    if ((candidates.size >= 2) && {
      val theirPt = candidates.tail.head.pt
      ourPt =:= theirPt
    }) {
      debug(s"no, because: ourPt = $ourPt, theirPt = ${candidates.tail.head.pt}")
      // c.diverge()
      c.abort(c.enclosingPosition, "stepping aside: repeating itself")
    } else {
      debug(s"not sure, need to explore alternatives")
      c.inferImplicitValue(ourPt, silent = true) match {
        case success if success != EmptyTree =>
          debug(s"no, because there's $success")
          c.abort(c.enclosingPosition, "stepping aside: there are other candidates")
          // c.diverge()
        case _ =>
          debug("yes, there are no obstacles. entering " + ourPt)
          val result = body
          debug("result: " + result)
          result
      }
    }
  }

  private var reflectivePrologueEmitted = false // TODO: come up with something better
  def reflectively(target: String, fir: FieldIR)(body: Tree => Tree): List[Tree] = reflectively(newTermName(target), fir)(body)

  def reflectivelyWithoutGetter(target: String, fir: FieldIR)(body: Tree => Tree): List[Tree] = {
    val pickleeName = newTermName(target)
    val getFieldValue = q"""
      val clazz = $pickleeName.getClass
      scala.util.Try(clazz.getDeclaredField(${fir.name})).map { javaField =>
        javaField.setAccessible(true)
        javaField.get($pickleeName)
      }
    """
    List(body(getFieldValue))
  }

  /**
   *  requires: !fir.accessor.isEmpty
   */
  def reflectively(target: TermName, fir: FieldIR)(body: Tree => Tree): List[Tree] = {
    val prologue = {
      if (!reflectivePrologueEmitted) {
        reflectivePrologueEmitted = true
        // TODO - Do we need the GRL for this?
        val initMirror = q"""
          val mirror = scala.reflect.runtime.universe.runtimeMirror(this.getClass.getClassLoader)
          val im = mirror.reflect($target)
        """.asInstanceOf[Block]
        initMirror.stats :+ initMirror.expr
      } else {
        Nil
      }
    }
    // val field = fir.field.get
    val owner = if (fir.param.nonEmpty) fir.param.get.owner
      else fir.accessor.get.owner
    val ownerSymbol = c.fresh(newTermName(fir.name + "Owner"))
    val firSymbol = c.fresh(newTermName(fir.name + "Symbol"))
    // TODO: make sure this works for:
    // 1) private[this] fields
    // 2) inherited private[this] fields
    // 3) overridden fields
    val wrappedBody =
      q"""
        val $ownerSymbol = implicitly[scala.pickling.FastTypeTag[${owner.asClass.toType.erasure}]].tpe
        val $firSymbol = $ownerSymbol.member(scala.reflect.runtime.universe.newTermName(${fir.name}))
        if ($firSymbol.isTerm) ${body(q"im.reflectField($firSymbol.asTerm)")}
      """.asInstanceOf[Block]
    prologue ++ wrappedBody.stats :+ wrappedBody.expr
  }
}

case class Hints(
  knownSize: Int = -1,
  elidedType: Option[FastTypeTag[_]] = None,
  oid: Int = -1,
  pinned: Boolean = false) {
  /** Returns true if the type tag can be/was eldied . */
  def isElidedType = !elidedType.isEmpty
  /** Returns true if the currently pickled object was previously pickled and can just be `shared` with the previous value. */
  def isSharedReference: Boolean = oid != -1
}

trait PickleTools extends Hintable {
  protected var hints: List[Hints] = List(Hints())
  def areHintsPinned: Boolean = hints.head.pinned

  override def hintKnownSize(knownSize: Int): this.type = {
    hints = hints.head.copy(knownSize = knownSize) :: hints.tail
    this
  }

  override def hintElidedType(tag: FastTypeTag[_]): this.type = {
    hints = hints.head.copy(elidedType = Some(tag)) :: hints.tail
    this
  }

  override def hintOid(oid: Int): this.type = {
    hints = hints.head.copy(oid = oid) :: hints.tail
    this
  }

  override def pinHints(): this.type = {
    hints = hints.head.copy(pinned = true) :: hints.tail
    this
  }

  override def unpinHints(): this.type = {
    hints = hints.head.copy(pinned = false) :: hints.tail
    this
  }

  override def pushHints(): this.type = {
    hints = Hints() :: hints
    this
  }

  override def popHints(): this.type = {
    hints = hints.tail
    this
  }

  def withHints[T](body: Hints => T): T = {
    val hints = this.hints.head
    if (!hints.pinned) this.hints = Hints() :: this.hints.tail
    body(hints)
  }
}
