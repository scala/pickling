package scala

import scala.language.experimental.macros
import scala.language.reflectiveCalls

import scala.reflect.runtime.universe._
import scala.annotation.implicitNotFound

package object pickling {

  // TOGGLE DEBUGGING
  var debugEnabled: Boolean = System.getProperty("pickling.debug", "false").toBoolean
  def debug(output: => String) = if (debugEnabled) println(output)

  implicit class PickleOps[T](picklee: T) {
    def pickle(implicit format: PickleFormat): Pickle = macro Compat.PickleMacros_pickle[T]
    def pickleInto(builder: PickleBuilder): Unit = macro Compat.PickleMacros_pickleInto[T]
  }

  implicit class RichSymbol(sym: scala.reflect.api.Symbols#Symbol) {
    def isEffectivelyFinal = sym.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isEffectivelyFinal
    def isEffectivelyPrimitive = throw new Exception("use Type.isEffectivelyPrimitive instead")
    def isNotNull = sym.asType.toType.asInstanceOf[scala.reflect.internal.Types#Type].isNotNull
  }

  var currentMirror: reflect.runtime.universe.Mirror = null

  def mirror: reflect.runtime.universe.Mirror =
    if (currentMirror == null) {
      currentMirror = reflect.runtime.currentMirror
      currentMirror
    } else currentMirror

  def typeToString(tpe: Type): String = tpe.key

  def typeFromString(mirror: Mirror, stpe: String): Type = {
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

  // FIXME: duplication wrt Tools, but I don't really fancy abstracting away this path-dependent madness
  implicit class RichType(tpe: Type) {
    import definitions._
    def key: String = {
      tpe match {
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

  def fastTypeTag[T: FastTypeTag] = implicitly[FastTypeTag[T]]

  implicit class RichFieldMirror(fm: FieldMirror) {
    // workaround for SI-7464
    def forcefulSet(value: Any): Unit = {
      import java.lang.reflect.{Field => jField}
      val jfield = fm.asInstanceOf[{ def jfield: jField }].jfield
      jfield.set(fm.receiver, value)
    }
  }
}

package pickling {

  @implicitNotFound(msg = "Cannot generate a pickler for ${T}. Recompile with -Xlog-implicits for details")
  trait Pickler[T] {
    val format: PickleFormat
    def pickle(picklee: T, builder: PickleBuilder): Unit
  }

  trait GenPicklers {
    implicit def genPickler[T](implicit format: PickleFormat): Pickler[T] = macro Compat.PicklerMacros_impl[T]
    // TODO: the primitive pickler hack employed here is funny, but I think we should fix this one
    // since people probably would also have to deal with the necessity to abstract over pickle formats
    def genPickler(classLoader: ClassLoader, clazz: Class[_])(implicit format: PickleFormat): Pickler[_] = {
      println(s"generating runtime pickler for $clazz") // NOTE: needs to be an explicit println, so that we don't occasionally fallback to runtime in static cases
      //val runtime = new CompiledPicklerRuntime(classLoader, clazz)
      val runtime = new InterpretedPicklerRuntime(classLoader, clazz)
      runtime.genPickler
    }
  }

  object Pickler extends CorePicklersUnpicklers

  @implicitNotFound(msg = "Cannot generate an unpickler for ${T}. Recompile with -Xlog-implicits for details")
  trait Unpickler[T] {
    val format: PickleFormat
    def unpickle(tag: => FastTypeTag[_], reader: PickleReader): Any
  }

  trait GenUnpicklers {
    implicit def genUnpickler[T](implicit format: PickleFormat): Unpickler[T] = macro Compat.UnpicklerMacros_impl[T]
    def genUnpickler(mirror: Mirror, tag: FastTypeTag[_])(implicit format: PickleFormat): Unpickler[_] = {
      println(s"generating runtime unpickler for ${tag.tpe}") // NOTE: needs to be an explicit println, so that we don't occasionally fallback to runtime in static cases
      //val runtime = new CompiledUnpicklerRuntime(mirror, tag)
      val runtime = new InterpretedUnpicklerRuntime(mirror, tag)
      runtime.genUnpickler
    }
  }

  object Unpickler extends CorePicklersUnpicklers

  trait Pickle {
    type ValueType
    val value: ValueType

    type PickleFormatType <: PickleFormat
    def unpickle[T] = macro Compat.UnpickleMacros_pickleUnpickle[T]
  }

  trait PickleFormat {
    type PickleType <: Pickle
    def createBuilder(): PickleBuilder
    def createReader(pickle: PickleType, mirror: Mirror): PickleReader
  }

  trait Hintable {
    def hintTag(tag: FastTypeTag[_]): this.type
    def hintKnownSize(knownSize: Int): this.type
    def hintStaticallyElidedType(): this.type
    def hintDynamicallyElidedType(): this.type
    def pinHints(): this.type
    def unpinHints(): this.type
  }

  trait PickleBuilder extends Hintable {
    def beginEntry(picklee: Any): this.type
    def putField(name: String, pickler: this.type => Unit): this.type
    def endEntry(): Unit
    def beginCollection(length: Int): this.type
    def putElement(pickler: this.type => Unit): this.type
    def endCollection(length: Int): Unit
    def result(): Pickle
  }

  trait PickleReader extends Hintable {
    def mirror: Mirror
    def beginEntry(): FastTypeTag[_]
    def beginEntryNoTag(): String
    def atPrimitive: Boolean
    def readPrimitive(): Any
    def atObject: Boolean
    def readField(name: String): PickleReader
    def endEntry(): Unit
    def beginCollection(): PickleReader
    def readLength(): Int
    def readElement(): PickleReader
    def endCollection(): Unit
    def unpickle[T]: T = macro Compat.UnpickleMacros_readerUnpickle[T]
    def unpickleTopLevel[T]: T = macro Compat.UnpickleMacros_readerUnpickleTopLevel[T]
  }

  case class PicklingException(msg: String) extends Exception(msg)
}
