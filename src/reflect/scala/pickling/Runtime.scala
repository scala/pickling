package scala.pickling

import scala.reflect.runtime.universe._
import definitions._
import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.ToolBox
import ir._

abstract class PicklerRuntime(classLoader: ClassLoader, clazz: Class[_]) {
  val mirror = runtimeMirror(classLoader)
  val sym = mirror.classSymbol(clazz)
  if (sym.typeParams.nonEmpty)
    throw new PicklingException(s"TODO: cannot pickle polymorphic types yet ($clazz)")
  val tpe = sym.toType
  val irs = new IRs[ru.type](ru)
  import irs._
  val cir = classIR(tpe)
}

class CompiledPicklerRuntime(classLoader: ClassLoader, clazz: Class[_]) extends PicklerRuntime(classLoader, clazz) {
  def genPickler(implicit format: PickleFormat): Pickler[_] = {
    // TODO: we should somehow cache toolboxes. maybe even inside the reflection API
    // TODO: toolbox bug. if we don't explicitly import PickleOps, it will fail to be found
    // more precisely: it will be found, but then immediately discarded, because a reference to it won't typecheck
    val formatTpe = mirror.reflect(format).symbol.asType.toType
    mirror.mkToolBox().eval(q"""
      import scala.pickling._
      import scala.pickling.`package`.PickleOps
      implicit val format: $formatTpe = new $formatTpe()
      implicitly[Pickler[$tpe]]
    """).asInstanceOf[Pickler[_]]
  }
}

// TODO: didn't test this one yet
class InterpretedPicklerRuntime(classLoader: ClassLoader, clazz: Class[_]) extends PicklerRuntime(classLoader, clazz) {
  def genPickler(implicit format: PickleFormat, p1: Pickler[Int], p2: Pickler[String]) = {
    if (tpe <:< typeOf[Int])         implicitly[Pickler[Int]]
    else if (tpe <:< typeOf[String]) implicitly[Pickler[String]]
    else {
      // build "interpreted" runtime pickler
      val format0 = format
      new Pickler[Any] {
        type PickleFormatType = PickleFormat
        implicit val format = format0
        type PickleBuilderType = PickleBuilder
        def pickle(picklee: Any, builder: PickleBuilder): Unit = {
          if (picklee != null) {
            val im = mirror.reflect(picklee)
            builder.beginEntry(TypeTag(tpe), picklee)
            cir.fields.foreach(fir => {
              if (!fir.hasGetter)
                throw new PicklingException(s"TODO: cannot pickle erased params yet (${fir.name} in $tpe)")
              val fldAccessor = tpe.declaration(TermName(fir.name)).asMethod
              val fldMirror   = im.reflectMethod(fldAccessor)
              val fldValue    = fldMirror()
              debug("pickling field value: " + fldValue)
              val fldClass    = if (fldValue != null) fldValue.getClass else mirror.runtimeClass(NullTpe)
              val fldPickler  = Pickler.genPickler(classLoader, fldClass).asInstanceOf[Pickler[_] { type PickleBuilderType = builder.type }]
              builder.putField(fir.name, b => fldPickler.pickle(fldValue, b))
            })
            builder.endEntry()
          } else {
            builder.beginEntry(TypeTag(NullTpe), null)
            builder.endEntry()
          }
        }
      }
    }
  }
}

// TODO: copy/paste wrt CompiledPicklerRuntime
class CompiledUnpicklerRuntime(mirror: Mirror, tag: TypeTag[_]) {
  def genUnpickler(implicit format: PickleFormat): Unpickler[_] = {
    // see notes and todos in CompiledPicklerRuntime.genPickler
    val formatTpe = mirror.reflect(format).symbol.asType.toType
    mirror.mkToolBox().eval(q"""
      import scala.pickling._
      import scala.pickling.`package`.PickleOps
      implicit val format: $formatTpe = new $formatTpe()
      implicitly[Unpickler[$tag]]
    """).asInstanceOf[Unpickler[_]]
  }
}

// TODO: implement this one
class InterpretedUnpicklerRuntime(mirror: Mirror, tag: TypeTag[_]) {
  def genPickler(implicit format: PickleFormat, p1: Pickler[Int], p2: Pickler[String]): Unpickler[_] = {
    ???
  }
}
