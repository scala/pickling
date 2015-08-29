package scala.pickling

import java.util.concurrent.atomic.AtomicReference

import scala.language.experimental.macros
import scala.language.reflectiveCalls

import java.util.IdentityHashMap

import HasCompat._

package object internal {

  import scala.reflect.runtime.{universe => ru}
  import ru._
  import compat._

  private[this] def initDefaultRuntime = {
    // TODO - Figure out some way to configure the default runtime at startup.
    if(true) new DefaultRuntime()
    else new NoReflectionRuntime()
  }
  private[this] var currentRuntimeVar = new AtomicReference[spi.PicklingRuntime](initDefaultRuntime)
  def currentRuntime: spi.PicklingRuntime = currentRuntimeVar.get
  // Here we inject a new runtime for usage.
  def replaceRuntime(r: spi.PicklingRuntime): Unit = {
    currentRuntimeVar.lazySet(r)
  }

  /* Global reflection lock.
   * It is used to avoid data races that typically lead to runtime exceptions
   * when using (Scala) runtime reflection on Scala 2.10.x.
   *
   * Note: visibility must be public, so that the lock can be accessed from
   *       macro-generated code.
   */
  def GRL = currentRuntime.GRL

  // TOGGLE DEBUGGING
  private val debugEnabled: Boolean = System.getProperty("pickling.debug", "false").toBoolean
  private[pickling] def debug(output: => String) = if (debugEnabled) println(output)


  // ----- internal extension methods for symbols -----
  private[pickling] implicit class RichSymbol(sym: scala.reflect.api.Universe#Symbol) {
    def isEffectivelyFinal = sym.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isEffectivelyFinal
    def isEffectivelyPrimitive = throw new Exception("use Type.isEffectivelyPrimitive instead")
    def isNotNullable = sym.isClass && (sym.asClass.isPrimitive || sym.asClass.isDerivedValueClass)
    def isNullable = sym.isClass && !isNotNullable
  }
  def currentMirror: ru.Mirror = currentRuntime.currentMirror

  private[pickling] def typeToString(tpe: Type): String = tpe.key

  private val typeFromStringCache = scala.collection.concurrent.TrieMap[String, Type]()
  private[pickling] def typeFromString(mirror: Mirror, stpe: String): Type = {
    // TODO: find out why typeFromString is called repeatedly for scala.Predef.String (at least in the evactor1 bench)
    if (typeFromStringCache.contains(stpe)) typeFromStringCache(stpe)
    else {
      val result =
        AppliedType.parseFull(stpe) match {
          case Some(AppliedType(typename, appliedTypeArgs)) =>
            def errorMsg = s"""error: cannot find class or module with type name '$typename'
                              |full type string: '$stpe'""".stripMargin

            val sym = try {
              if (typename.endsWith(".type")) mirror.staticModule(typename.stripSuffix(".type")).moduleClass
              else mirror.staticClass(typename)
            } catch {
              case _: ScalaReflectionException =>
                sys.error(errorMsg)
              case _: scala.reflect.internal.MissingRequirementError =>
                sys.error(errorMsg)
            }
            val tycon = sym.asType.toTypeConstructor
            appliedType(tycon, appliedTypeArgs.map(starg => typeFromString(mirror, starg.toString)))
          case None =>
            sys.error(s"fatal: cannot unpickle $stpe")
        }
      typeFromStringCache(stpe) = result
      result
    }
  }

  // FIXME: duplication wrt Tools, but I don't really fancy abstracting away this path-dependent madness
  private[pickling] implicit class RichTypeFIXME(tpe: Type) {
    import definitions._
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
    def isEffectivelyPrimitive: Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && eltpe.typeSymbol.isClass && eltpe.typeSymbol.asClass.isPrimitive => true
      case _ => false
    }
  }


  // ----- utilities for managing object identity -----
  // TODO - deprecated all of these, or leave them for convenience?
  def lookupPicklee(picklee: Any): Int = currentRuntime.refRegistry.pickle.registerPicklee(picklee)
  def registerPicklee(picklee: Any) = currentRuntime.refRegistry.pickle.registerPicklee(picklee)
  def clearPicklees() = currentRuntime.refRegistry.pickle.clear()
  def lookupUnpicklee(index: Int): Any = currentRuntime.refRegistry.unpickle.lookupUnpicklee(index)
  def preregisterUnpicklee() = currentRuntime.refRegistry.unpickle.preregisterUnpicklee()
  def registerUnpicklee(unpicklee: Any, index: Int) = currentRuntime.refRegistry.unpickle.regsiterUnpicklee(index, unpicklee)

  def clearUnpicklees() = currentRuntime.refRegistry.unpickle.clear()
}
