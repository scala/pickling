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

  def compileTimeDispatchees(sym: Symbol, mirror: Mirror): List[Symbol] = {
    val subclasses = allStaticallyKnownConcreteSubclasses(sym, mirror)
    val blackList = Set[Symbol](AnyClass, AnyRefClass, AnyValClass)
    val whiteList = (sym: Symbol) => sym.asClass.isPrimitive
    val inclusive = sym.isClass && !blackList(sym) && (whiteList(sym) || (!sym.asClass.isAbstractClass && !sym.asClass.isTrait))
    subclasses ++ (if (inclusive) List(sym) else Nil)
  }

  def allStaticallyKnownConcreteSubclasses(sym: Symbol, mirror: Mirror): List[Symbol] = {
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
          if (bc != c && bc != AnyClass && bc != AnyRefClass && bc != AnyValClass) // TODO: what else do we want to ignore?
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
            pkg.name.toString != "jansi" // TODO: and another one (jline.jar)
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
    else {
      var unsorted =
        u match {
          case u: scala.tools.nsc.Global if u.currentRun.compiles(sym.asInstanceOf[u.Symbol]) => sourcepathScan()
          case _ => sourcepathAndClasspathScan()
        }
      // NOTE: need to order the list: children first, parents last
      // otherwise pattern match which uses this list might work funnily
      unsorted.distinct.sortWith((c1, c2) => c1.asClass.baseClasses.contains(c2))
    }
  }
}

abstract class Macro extends scala.reflect.macros.Macro {
  import c.universe._
  import definitions._

  val tools = new Tools[c.universe.type](c.universe)
  val irs = new ir.PickleIRs[c.universe.type](c.universe)

  def instantiatePickleFormat(pickleFormat: Tree): PickleFormat = {
    def failPickleFormat(msg: String) = c.abort(c.enclosingPosition, s"$msg for ${pickleFormat} of type ${pickleFormat.tpe}")
    val pickleFormatCarrier = c.typeCheck(q"$pickleFormat.instantiate", silent = true)
    pickleFormatCarrier.attachments.all.find(_.isInstanceOf[PickleFormat]) match {
      case Some(pf: PickleFormat) => pf
      case _ => failPickleFormat("Couldn't instantiate PickleFormat")
    }
  }

  def pickleType(pickleFormat: Tree): Type = {
    def failPickleFormat(msg: String) = c.abort(c.enclosingPosition, s"$msg for ${pickleFormat} of type ${pickleFormat.tpe}")
    val pickleTypeCarrier = c.typeCheck(tq"${pickleFormat.tpe}#PickleType", mode = c.TYPEmode, silent = true)
    pickleTypeCarrier match {
      case EmptyTree => failPickleFormat("Couldn't resolve PickleType")
      case tree => tree.tpe.normalize match {
        case tpe if tpe.typeSymbol.isClass => tpe
        case tpe => failPickleFormat(s"PickleType resolved as $tpe is invalid")
      }
    }
  }

  def pickleFormatType(pickleTree: Tree): Type = {
    def failUnpickle(msg: String) = c.abort(c.enclosingPosition, s"$msg for $pickleTree of type ${pickleTree.tpe}")
    val pickleFormatTypeCarrier = c.typeCheck(tq"${pickleTree.tpe}#PickleFormatType", mode = c.TYPEmode, silent = true)
    pickleFormatTypeCarrier match {
      case EmptyTree => failUnpickle("Couldn't resolve PickleFormatType")
      case tree => tree.tpe.normalize match {
        case tpe if tpe.typeSymbol.isClass => tpe
        case tpe => failUnpickle(s"PickleFormatType resolved as $tpe is invalid")
      }
    }
  }

  def compileTimeDispatchees(tpe: Type): List[Type] = {
    tools.compileTimeDispatchees(tpe.typeSymbol, rootMirror).map(_.asType.toType)
  }
}