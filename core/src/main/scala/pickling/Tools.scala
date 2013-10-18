package scala.pickling

import scala.pickling.internal._

import scala.language.existentials

import scala.reflect.macros.Context
import scala.reflect.api.Universe
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

import scala.collection.mutable.{Map => MutableMap, ListBuffer => MutableList, WeakHashMap, Set => MutableSet}
import scala.collection.mutable.{Stack => MutableStack, Queue => MutableQueue}

import java.lang.ref.WeakReference

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

  // TODO: the standard "c.topLevelRef orElse c.introduceTopLevel" approach doesn't work with the runtime compiler
  // hence we should go for this hack. at least it's not going to OOM us...
  val generatedNames = MutableSet[Any]()
}

class Tools[C <: Context](val c: C) {
  val u: c.universe.type = c.universe
  import u._
  import definitions._

  def blackList(sym: Symbol) = sym == AnyClass || sym == AnyRefClass || sym == AnyValClass || sym == ObjectClass

  def isRelevantSubclass(baseSym: Symbol, subSym: Symbol) = {
    !blackList(baseSym) && !blackList(subSym) && subSym.isClass && {
      val subClass = subSym.asClass
      subClass.baseClasses.contains(baseSym) && !subClass.isAbstractClass && !subClass.isTrait
    }
  }

  def compileTimeDispatchees(tpe: Type, mirror: Mirror): List[Type] = {
    // TODO: why do we need nullTpe?
    val nullTpe = if (tpe.baseClasses.contains(ObjectClass)) List(NullTpe) else Nil
    val subtypes = allStaticallyKnownConcreteSubclasses(tpe, mirror).filter(subtpe => subtpe.typeSymbol != tpe.typeSymbol)
    val selfTpe = if (isRelevantSubclass(tpe.typeSymbol, tpe.typeSymbol)) List(tpe) else Nil
    val result = nullTpe ++ subtypes ++ selfTpe
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
        // NOTE: only looking for top-level classes!
        case PackageDef(_, stats) => stats.foreach(loop)
        case cdef: ClassDef => analyze(cdef.symbol)
        case mdef: ModuleDef => analyze(mdef.symbol.asModule.moduleClass)
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
          } else if (sym.isSealed) {
            val syms: List[ClassSymbol] =
              sym.knownDirectSubclasses.toList.map {
                case csym: ClassSymbol => csym
                case msym: ModuleSymbol => msym.moduleClass.asClass
                case osym => throw new Exception(s"unexpected known direct subclass: $osym <: $sym")
              }.flatMap(loop)
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
        if (baseSym.isClass && baseSym.asClass.isSealed) sealedHierarchyScan()
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

abstract class ShareAnalyzer[U <: Universe](val u: U) {
  import u._
  import definitions._

  val irs = new ir.IRs[u.type](u)
  import irs._

  // FIXME: duplication wrt pickling.`package`, but I don't really fancy abstracting away this path-dependent madness
  implicit class RichTypeFIXME(tpe: Type) {
    def isEffectivelyPrimitive: Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && eltpe.isEffectivelyPrimitive => true
      case _ => false
    }
  }

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
            val more = flattenedClassIR(currTpe).fields.map(_.tpe)
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

abstract class Macro extends Reflection211Compat { self =>
  val c: Context
  import c.universe._
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

  private def innerType(target: Tree, name: String): Type = {
    def fail(msg: String) = c.abort(c.enclosingPosition, s"$msg for ${target} of type ${target.tpe}")
    // val carrier = c.typeCheck(tq"${target.tpe}#${TypeName(name)}", mode = c.TYPEmode, silent = true)
    val carrier = c.typeCheck(q"{ val x: ${target.tpe}#${TypeName(name)} = ??? }", silent = true)
    carrier match {
      case EmptyTree => fail(s"Couldn't resolve $name")
      case Block(ValDef(_, _, tpt, _) :: _, _) => tpt.tpe.normalize match {
        case tpe if tpe.typeSymbol.isClass => tpe
        case tpe => fail(s"$name resolved as $tpe is invalid")
      }
    }
  }

  // FIXME: duplication wrt pickling.`package`, but I don't really fancy abstracting away this path-dependent madness
  implicit class RichTypeFIXME(tpe: Type) {
    def key: String = {
      tpe.normalize match {
        case ExistentialType(tparams, TypeRef(pre, sym, targs))
        if targs.nonEmpty && targs.forall(targ => tparams.contains(targ.typeSymbol)) =>
          TypeRef(pre, sym, Nil).key
        case TypeRef(pre, sym, targs) if pre.typeSymbol.isModuleClass =>
          sym.fullName +
          (if (sym.isModuleClass) ".type" else "") +
          (if (targs.isEmpty) "" else targs.map(_.key).mkString("[", ",", "]"))
        case _ =>
          tpe.toString
      }
    }
    def canCauseLoops: Boolean = shareAnalyzer.canCauseLoops(tpe)
    def isEffectivelyPrimitive: Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && eltpe.isEffectivelyPrimitive => true
      case _ => false
    }
  }

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

  def pickleFormatType(pickle: Tree): Type = innerType(pickle, "PickleFormatType")

  def compileTimeDispatchees(tpe: Type): List[Type] = tools.compileTimeDispatchees(tpe, rootMirror)

  def syntheticPackageName: String = "scala.pickling.synthetic"
  def syntheticBaseName(tpe: Type): TypeName = {
    val raw = tpe.key.split('.').map(_.capitalize).mkString("")
    val encoded = TypeName(raw).encoded
    TypeName(encoded)
  }
  def syntheticBaseQualifiedName(tpe: Type): TypeName = TypeName(syntheticPackageName + "." + syntheticBaseName(tpe).toString)

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
    def debug(msg: Any) = {
      val padding = "  " * (c.enclosingImplicits.length - 1)
      // Console.err.println(padding + msg)
    }
    debug("can we enter " + c.enclosingImplicits.head._1 + "?")
    debug(c.enclosingImplicits)
    c.enclosingImplicits match {
      case (ourPt, _) :: (theirPt, _) :: _ if ourPt =:= theirPt =>
        debug(s"no, because: ourPt = $ourPt, theirPt = $theirPt")
        // c.diverge()
        c.abort(c.enclosingPosition, "stepping aside: repeating itself")
      case _ =>
        debug(s"not sure, need to explore alternatives")
        c.inferImplicitValue(c.enclosingImplicits.head._1, silent = true) match {
          case success if success != EmptyTree =>
            debug(s"no, because there's $success")
            c.abort(c.enclosingPosition, "stepping aside: there are other candidates")
            // c.diverge()
          case _ =>
            debug("yes, there are no obstacles. entering " + c.enclosingImplicits.head._1)
            val result = body
            debug("result: " + result)
            result
        }
    }
  }

  private var reflectivePrologueEmitted = false // TODO: come up with something better
  def reflectively(target: String, fir: FieldIR)(body: Tree => Tree): List[Tree] = reflectively(TermName(target), fir)(body)

  /**
   *  requires: !fir.accessor.isEmpty
   */
  def reflectively(target: TermName, fir: FieldIR)(body: Tree => Tree): List[Tree] = {
    val prologue = {
      if (!reflectivePrologueEmitted) {
        reflectivePrologueEmitted = true
        val initMirror = q"""
          import scala.reflect.runtime.universe._
          val mirror = runtimeMirror(this.getClass.getClassLoader)
          val im = mirror.reflect($target)
        """.asInstanceOf[Block]
        initMirror.stats :+ initMirror.expr
      } else {
        Nil
      }
    }
    val field = fir.field.get
    val ownerSymbol = TermName(fir.name + "Owner")
    val firSymbol = TermName(fir.name + "Symbol")
    // TODO: make sure this works for:
    // 1) private[this] fields
    // 2) inherited private[this] fields
    // 3) overridden fields
    val wrappedBody =
      q"""
        val $ownerSymbol = implicitly[scala.pickling.FastTypeTag[${field.owner.asClass.toType.erasure}]].tpe
        val $firSymbol = $ownerSymbol.member(newTermName(${field.name.toString}))
        if ($firSymbol.isTerm) ${body(q"im.reflectField($firSymbol.asTerm)")}
      """.asInstanceOf[Block]
    prologue ++ wrappedBody.stats :+ wrappedBody.expr
  }
}

case class Hints(
  tag: FastTypeTag[_] = null,
  knownSize: Int = -1,
  isStaticallyElidedType: Boolean = false,
  isDynamicallyElidedType: Boolean = false,
  oid: Int = -1) {
  def isElidedType = isStaticallyElidedType || isDynamicallyElidedType
}

trait PickleTools {
  var hints = new Hints()
  var areHintsPinned = false

  def hintTag(tag: FastTypeTag[_]): this.type = { hints = hints.copy(tag = tag); this }
  def hintKnownSize(knownSize: Int): this.type = { hints = hints.copy(knownSize = knownSize); this }
  def hintStaticallyElidedType(): this.type = { hints = hints.copy(isStaticallyElidedType = true); this }
  def hintDynamicallyElidedType(): this.type = { hints = hints.copy(isDynamicallyElidedType = true); this }
  def hintOid(oid: Int): this.type = { hints = hints.copy(oid = oid); this }
  def pinHints(): this.type = { areHintsPinned = true; this }
  def unpinHints(): this.type = { areHintsPinned = false; hints = new Hints(); this }

  def withHints[T](body: Hints => T): T = {
    val hints = this.hints
    if (!areHintsPinned) this.hints = new Hints
    body(hints)
  }
}

trait CurrentMirrorMacro extends Macro {
  def impl: c.Tree = {
    import c.universe._
    c.inferImplicitValue(typeOf[ru.Mirror], silent = true) orElse {
      val cachedMirror = q"scala.pickling.internal.`package`.cachedMirror"
      q"""
        if ($cachedMirror != null) $cachedMirror
        else {
          $cachedMirror = scala.reflect.runtime.currentMirror
          $cachedMirror
        }
      """
    }
  }
}
