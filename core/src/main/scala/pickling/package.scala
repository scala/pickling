package scala

import scala.language.experimental.macros

import scala.reflect.runtime.{universe => ru}
import ru._

package object pickling {

  // TOGGLE DEBUGGING
  var debugEnabled: Boolean = System.getProperty("pickling.debug", "false").toBoolean
  def debug(output: => String) = if (debugEnabled) println(output)

  implicit class PickleOps[T](picklee: T) {
    def pickle(implicit format: PickleFormat): format.PickleType = macro Compat.PickleMacros_pickle[T]
    def pickleInto(builder: PBuilder): Unit = macro Compat.PickleMacros_pickleInto[T]
    def pickleTo(output: Output[_])(implicit format: PickleFormat): Unit = macro Compat.PickleMacros_pickleTo[T]
  }

  implicit class RichSymbol(sym: scala.reflect.api.Universe#Symbol) {
    def isEffectivelyFinal = sym.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isEffectivelyFinal
    def isEffectivelyPrimitive = throw new Exception("use Type.isEffectivelyPrimitive instead")
    def isNotNullable = sym.isClass && (sym.asClass.isPrimitive || sym.asClass.isDerivedValueClass)
    def isNullable = sym.isClass && !isNotNullable
  }

  implicit class RichType(tpe: scala.reflect.api.Universe#Type) {
    def isEffectivelyFinal = tpe.typeSymbol.isEffectivelyFinal
    // TODO: doesn't work...
    // def isEffectivelyPrimitive: Boolean = {
    //   tpe.typeSymbol.isPrimitive || {
    //     val args = tpe.asInstanceOf[scala.reflect.internal.SymbolTable#Type].typeArguments
    //     def isArrayOfSomething = tpe.toString.startsWith("scala.Array[") || tpe.toString.startsWith("Array[")
    //     def isParameterizedByPrimitive = args.nonEmpty && args.head.isEffectivelyPrimitive
    //     isArrayOfSomething && isParameterizedByPrimitive
    //   }
    // }
    def isNotNullable = tpe.typeSymbol.isNotNullable
    def isNullable = tpe.typeSymbol.isNullable
  }

  var cachedMirror: ru.Mirror = null
  def currentMirror: ru.Mirror = macro Compat.CurrentMirrorMacro_impl

  def typeToString(tpe: Type): String = tpe.key

  private val typeFromStringCache = scala.collection.mutable.Map[String, Type]()
  def typeFromString(mirror: Mirror, stpe: String): Type = {
    // TODO: find out why typeFromString is called repeatedly for scala.Predef.String (at least in the evactor1 bench)
    if (typeFromStringCache.contains(stpe)) typeFromStringCache(stpe)
    else {
      val result = {
        val (ssym, stargs) = {
          val Pattern = """^(.*?)(\[(.*?)\])?$""".r
          def fail() = throw new PicklingException(s"fatal: cannot unpickle $stpe")
          stpe match {
            case Pattern("", _, _) => fail()
            case Pattern(sym, _, null) => (sym, Nil)
            case Pattern(sym, _, stargs) => (sym, stargs.split(",").map(_.trim).toList)
            case _ => fail()
          }
        }

        val sym = if (ssym.endsWith(".type")) mirror.staticModule(ssym.stripSuffix(".type")).moduleClass else mirror.staticClass(ssym)
        val tycon = sym.asType.toTypeConstructor
        appliedType(tycon, stargs.map(starg => typeFromString(mirror, starg)))
      }
      typeFromStringCache(stpe) = result
      result
    }
  }

  // FIXME: duplication wrt Tools, but I don't really fancy abstracting away this path-dependent madness
  implicit class RichTypeFIXME(tpe: Type) {
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
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && eltpe.isEffectivelyPrimitive => true
      case _ => false
    }
  }

  implicit class RichFieldMirror(fm: FieldMirror) {
    // workaround for SI-7464
    def forcefulSet(value: Any): Unit = {
      import java.lang.reflect.{Field => jField}
      val jfield = fm.asInstanceOf[{ def jfield: jField }].jfield
      jfield.set(fm.receiver, value)
    }
  }

  private var picklees = new ReactMap
  private var nextPicklee = 0
  def lookupPicklee(picklee: Any) = {
    val index = nextPicklee
    val result = picklees.insertIfNotThere(picklee.asInstanceOf[AnyRef], index)
    // println(s"lookupPicklee($picklee) = $result")
    if (result == -1)
      nextPicklee += 1
    result
  }
  def registerPicklee(picklee: Any) = {
    val index = nextPicklee
    picklees.insert(picklee.asInstanceOf[AnyRef], index)
    // println(s"registerPicklee($picklee, $index)")
    nextPicklee += 1
    index
  }
  def clearPicklees() = {
    picklees.clear()
    nextPicklee = 0
  }

  private var unpicklees = new Array[Any](65536)
  private var nextUnpicklee = 0

  def lookupUnpicklee(index: Int): Any = {
    // println(s"lookupUnpicklee($index)")
    if (index >= nextUnpicklee) throw new Error(s"fatal error: invalid index $index in unpicklee cache of length $nextUnpicklee")
    val result = unpicklees(index)
    if (result == null) throw new Error(s"fatal error: unpicklee cache is corrupted at $index")
    result
  }
  def preregisterUnpicklee() = {
    val index = nextUnpicklee
    // TODO: dynamically resize the array!
    unpicklees(index) = null
    // println(s"preregisterUnpicklee() at $index")
    nextUnpicklee += 1
    index
  }
  def registerUnpicklee(unpicklee: Any, index: Int) = {
    // println(s"registerUnpicklee($unpicklee, $index)")
    unpicklees(index) = unpicklee
  }
  def clearUnpicklees() = {
    var i = 0
    while (i < nextUnpicklee) {
      unpicklees(i) = null
      i += 1
    }
    nextUnpicklee = 0
  }
}
