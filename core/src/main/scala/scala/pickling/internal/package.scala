package scala.pickling

import scala.language.experimental.macros
import scala.language.reflectiveCalls

import java.util.IdentityHashMap

import HasCompat._

package object internal {

  import scala.reflect.runtime.{universe => ru}
  import ru._
  import compat._

  /* Global reflection lock.
   * It is used to avoid data races that typically lead to runtime exceptions
   * when using (Scala) runtime reflection on Scala 2.10.x.
   *
   * Note: visibility must be public, so that the lock can be accessed from
   *       macro-generated code.
   */
  val GRL = new java.util.concurrent.locks.ReentrantLock

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

  var cachedMirror: ru.Mirror = null
  def currentMirror: ru.Mirror = macro Compat.CurrentMirrorMacro_impl

  private[pickling] def typeToString(tpe: Type): String = tpe.key

  private val typeFromStringCache = scala.collection.concurrent.TrieMap[String, Type]()
  private[pickling] def typeFromString(mirror: Mirror, stpe: String): Type = {
    // TODO: find out why typeFromString is called repeatedly for scala.Predef.String (at least in the evactor1 bench)
    if (typeFromStringCache.contains(stpe)) typeFromStringCache(stpe)
    else {
      val result =
        AppliedType.parse(stpe) match {
          case (AppliedType(typename, appliedTypeArgs), _) =>
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
          case _ =>
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
  private val pickleesTL = new ThreadLocal[IdentityHashMap[AnyRef, Integer]] {
    override def initialValue() = new IdentityHashMap[AnyRef, Integer]()
  }
  private val nextPickleeTL = new ThreadLocal[Int] {
    override def initialValue() = 0
  }

  def lookupPicklee(picklee: Any): Int = {
    val anyRefPicklee = picklee.asInstanceOf[AnyRef]
    // check if `anyRefPicklee` is already in the map.
    // if so, obtain its index, else insert at index `nextPicklee`.
    val picklees = pickleesTL.get()
    if (picklees.containsKey(anyRefPicklee)) {
      picklees.get(anyRefPicklee).intValue
    } else {
      val nextPicklee = nextPickleeTL.get()
      picklees.put(anyRefPicklee, new Integer(nextPicklee))
      nextPickleeTL.set(nextPicklee + 1)
      -1
    }
  }

  def registerPicklee(picklee: Any) = {
    var nextPicklee = nextPickleeTL.get()
    val picklees = pickleesTL.get()

    val index = nextPicklee
    picklees.put(picklee.asInstanceOf[AnyRef], new Integer(index))

    // println(s"registerPicklee($picklee, $index)")
    nextPicklee += 1
    nextPickleeTL.set(nextPicklee)
    pickleesTL.set(picklees)
    index
  }

  def clearPicklees() = {
    var nextPicklee = nextPickleeTL.get()
    val picklees = pickleesTL.get()

    picklees.clear()
    nextPicklee = 0

    nextPickleeTL.set(nextPicklee)
    pickleesTL.set(picklees)
  }

  private val unpickleesTL = new ThreadLocal[Array[Any]] {
    override def initialValue() = new Array[Any](65536)
  }
  private val nextUnpickleeTL = new ThreadLocal[Int] {
    override def initialValue() = 0
  }

  def lookupUnpicklee(index: Int): Any = {
    val nextUnpicklee = nextUnpickleeTL.get()
    val unpicklees = unpickleesTL.get()

    // println(s"lookupUnpicklee($index)")
    if (index >= nextUnpicklee) throw PicklingException(s"fatal error: invalid index $index in unpicklee cache of length $nextUnpicklee")
    val result = unpicklees(index)
    if (result == null) throw new Error(s"fatal error: unpicklee cache is corrupted at $index")
    result
  }

  def preregisterUnpicklee() = {
    val nextUnpicklee = nextUnpickleeTL.get()
    val index = nextUnpicklee

    val unpicklees = unpickleesTL.get()

    val len = unpicklees.length
    val target = if (index == len) {
      val newArr = Array.ofDim[Any](len * 2)
      System.arraycopy(unpicklees, 0, newArr, 0, len)
      unpickleesTL.set(newArr)
      newArr
    } else
      unpicklees
    target(index) = null

    // println(s"preregisterUnpicklee() at $index")
    nextUnpickleeTL.set(nextUnpicklee + 1)
    index
  }

  def registerUnpicklee(unpicklee: Any, index: Int) = {
    val unpicklees = unpickleesTL.get()

    // println(s"registerUnpicklee($unpicklee, $index)")
    unpicklees(index) = unpicklee
    unpickleesTL.set(unpicklees)
  }

  def clearUnpicklees() = {
    var nextUnpicklee = nextUnpickleeTL.get()
    val unpicklees = unpickleesTL.get()

    var i = 0
    while (i < nextUnpicklee) {
      unpicklees(i) = null
      i += 1
    }
    nextUnpicklee = 0
    nextUnpickleeTL.set(nextUnpicklee)
    unpickleesTL.set(unpicklees)
  }
}
