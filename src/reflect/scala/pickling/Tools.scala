package scala.pickling

import scala.reflect.api.Universe
import scala.reflect.macros.Context
import scala.collection.mutable.{Map => MutableMap, ListBuffer => MutableList, WeakHashMap}
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
}

class Tools[U <: Universe with Singleton](val u: U) {
  import u._
  import definitions._

  def blackList(sym: Symbol) = sym == AnyClass || sym == AnyRefClass || sym == AnyValClass

  def compileTimeDispatchees(tpe: Type, mirror: Mirror): List[Type] = {
    val subtypes = allStaticallyKnownConcreteSubclasses(tpe, mirror)
    def includeTpeItself = {
      val sym = tpe.typeSymbol
      val whiteList = (sym: Symbol) => sym.asClass.isPrimitive
      sym.isClass && !blackList(sym) && (whiteList(sym) || (!sym.asClass.isAbstractClass && !sym.asClass.isTrait))
    }
    subtypes ++ (if (includeTpeItself) List(tpe) else Nil)
  }

  def allStaticallyKnownConcreteSubclasses(tpe: Type, mirror: Mirror): List[Type] = {
    // TODO: so far the search is a bit dumb
    // given `class C[T]; class D extends C[Int]` and `tpe = C[String]`, it will return <symbol of D>
    // TODO: on a more elaborate note
    // given `class C; class D[T] extends C` we of course cannot return the infinite number of `D[X]` types
    // but what we can probably do is to additionally look up custom picklers/unpicklers of for specific `D[X]`
    val sym = tpe.typeSymbol

    def sourcepathScan(): List[Symbol] = {
      val g = u.asInstanceOf[scala.tools.nsc.Global]
      val subclasses = MutableList[g.Symbol]()
      def loop(tree: g.Tree): Unit = tree match {
        case g.PackageDef(_, stats) => stats.foreach(loop)
        case implDef: g.ImplDef if implDef.symbol != sym && implDef.symbol.baseClasses.contains(sym) => subclasses += implDef.symbol
        case _ => // do nothing
      }
      g.currentRun.units.map(_.body).foreach(loop)
      subclasses.toList.asInstanceOf[List[u.Symbol]]
    }

    def sourcepathAndClasspathScan(): List[Symbol] = {
      lazy val classpathCache = Tools.subclassCache(mirror, {
        val cache = MutableMap[Symbol, MutableList[Symbol]]()
        def updateCache(bc: Symbol, c: Symbol) = {
          if (bc != c && !blackList(bc)) // TODO: what else do we want to ignore?
            cache.getOrElseUpdate(bc, MutableList()) += c
        }
        def loop(pkg: Symbol): Unit = {
          val pkgMembers = pkg.typeSignature.members
          pkgMembers foreach (m => {
            def analyze(m: Symbol): Unit = {
              if (m.name.decoded.contains("$")) () // SI-7251
              else if (m.isClass && !m.asClass.isAbstractClass && !m.asClass.isTrait) m.asClass.baseClasses foreach (bc => updateCache(bc, m))
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
      classpathCache.getOrElse(sym, Nil).toList
    }

    if (sym.isFinal) Nil
    else if (blackList(sym)) Nil
    else {
      var unsorted =
        u match {
          case u: scala.tools.nsc.Global if u.currentRun.compiles(sym.asInstanceOf[u.Symbol]) => sourcepathScan()
          case _ => sourcepathAndClasspathScan()
        }
      // NOTE: need to order the list: children first, parents last
      // otherwise pattern match which uses this list might work funnily
      val subSyms = unsorted.distinct.sortWith((c1, c2) => c1.asClass.baseClasses.contains(c2))
      subSyms map (sym => {
        val tpeWithMaybeTparams = sym.asType.toType
        val tparams = tpeWithMaybeTparams match {
          case PolyType(tparams, _) => tparams
          case _ => Nil
        }
        existentialAbstraction(tparams, tpeWithMaybeTparams)
      })
    }
  }
}

abstract class Macro extends scala.reflect.macros.Macro {
  import c.universe._
  import definitions._

  val tools = new Tools[c.universe.type](c.universe)
  import tools._

  val irs = new ir.IRs[c.universe.type](c.universe)
  import irs._

  private def innerType(target: Tree, name: String): Type = {
    def fail(msg: String) = c.abort(c.enclosingPosition, s"$msg for ${target} of type ${target.tpe}")
    val carrier = c.typeCheck(tq"${target.tpe}#${TypeName(name)}", mode = c.TYPEmode, silent = true)
    carrier match {
      case EmptyTree => fail(s"Couldn't resolve $name")
      case tree => tree.tpe.normalize match {
        case tpe if tpe.typeSymbol.isClass => tpe
        case tpe => fail(s"$name resolved as $tpe is invalid")
      }
    }
  }

  def pickleBuilderType(pickleFormat: Tree): Type = innerType(pickleFormat, "PickleBuilderType")
  def pickleReaderType(pickleFormat: Tree): Type = innerType(pickleFormat, "PickleReaderType")
  def pickleFormatType(pickle: Tree): Type = innerType(pickle, "PickleFormatType")

  def compileTimeDispatchees(tpe: Type): List[Type] = tools.compileTimeDispatchees(tpe, rootMirror)

  def syntheticPackageName: String = "scala.pickling.synthetic"
  def syntheticBaseName(tpe: Type): TypeName = TypeName(tpe.typeSymbol.fullName.split('.').map(_.capitalize).mkString(""))
  def syntheticBaseQualifiedName(tpe: Type): TypeName = TypeName(syntheticPackageName + "." + syntheticBaseName(tpe).toString)

  def syntheticPicklerName(tpe: Type, builderTpe: Type): TypeName = syntheticBaseName(tpe) + syntheticPicklerSuffix(builderTpe)
  def syntheticPicklerQualifiedName(tpe: Type, builderTpe: Type): TypeName = syntheticBaseQualifiedName(tpe) + syntheticPicklerSuffix(builderTpe)
  def syntheticPicklerSuffix(builderTpe: Type): String = builderTpe.typeSymbol.name.toString.stripSuffix("PickleBuilder") + "Pickler"

  def syntheticUnpicklerName(tpe: Type, readerTpe: Type): TypeName = syntheticBaseName(tpe) + syntheticUnpicklerSuffix(readerTpe)
  def syntheticUnpicklerQualifiedName(tpe: Type, readerTpe: Type): TypeName = syntheticBaseQualifiedName(tpe) + syntheticUnpicklerSuffix(readerTpe)
  def syntheticUnpicklerSuffix(readerTpe: Type): String = readerTpe.typeSymbol.name.toString.stripSuffix("PickleReader") + "Unpickler"

  def preferringAlternativeImplicits(body: => Tree): Tree = {
    def debug(msg: Any) = {
      val padding = "  " * (c.enclosingImplicits.length - 1)
      // Console.err.println(padding + msg)
    }
    debug("can we enter " + c.enclosingImplicits.head.pt + "?")
    debug(c.enclosingImplicits)
    c.enclosingImplicits match {
      case c.ImplicitCandidate(_, _, ourPt, _) :: c.ImplicitCandidate(_, _, theirPt, _) :: _ if ourPt =:= theirPt =>
        debug(s"no, because: ourPt = $ourPt, theirPt = $theirPt")
        c.diverge()
      case _ =>
        debug(s"not sure, need to explore alternatives")
        c.inferImplicitValue(c.enclosingImplicits.head.pt, silent = true) match {
          case success if success != EmptyTree =>
            debug(s"no, because there's $success")
            c.diverge()
          case _ =>
            debug("yes, there are no obstacles. entering " + c.enclosingImplicits.head.pt)
            val result = body
            debug("result: " + result)
            result
        }
    }
  }

  private var reflectivePrologueEmitted = false // TODO: come up with something better
  def reflectively(target: String, fir: FieldIR)(body: Tree => Tree): List[Tree] = reflectively(TermName(target), fir)(body)
  def reflectively(target: TermName, fir: FieldIR)(body: Tree => Tree): List[Tree] = {
    val prologue = {
      if (!reflectivePrologueEmitted) {
        reflectivePrologueEmitted = true
        val initMirror = q"""
          import scala.reflect.runtime.universe._
          val mirror = runtimeMirror(getClass.getClassLoader)
          val im = mirror.reflect($target)
        """
        initMirror.stats :+ initMirror.expr
      } else {
        Nil
      }
    }
    val field = fir.field.get
    val firSymbol = TermName(fir.name + "Symbol")
    // TODO: make sure this works for:
    // 1) private[this] fields
    // 2) inherited private[this] fields
    // 3) overridden fields
    val wrappedBody =
      q"""
        val $firSymbol = typeOf[${field.owner.asClass.toType}].member(TermName(${field.name.toString}))
        if ($firSymbol.isTerm) ${body(q"im.reflectField($firSymbol.asTerm)")}
      """
    prologue ++ wrappedBody.stats :+ wrappedBody.expr
  }
}