package scala.pickling

import scala.reflect.runtime.universe._
import definitions._
import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.ToolBox
import ir._

object Runtime {
  val toUnboxed = Map[Class[_], Class[_]](
    classOf[java.lang.Integer]       -> classOf[Int],
    classOf[java.lang.Long]          -> classOf[Long],
    classOf[java.lang.Float]         -> classOf[Float],
    classOf[java.lang.Double]        -> classOf[Double],
    classOf[java.lang.Short]         -> classOf[Short],
    classOf[java.lang.Character]     -> classOf[Char],
    classOf[java.lang.Byte]          -> classOf[Byte],
    classOf[scala.runtime.BoxedUnit] -> classOf[Unit],
    classOf[java.lang.Boolean]       -> classOf[Boolean],
    classOf[java.lang.String]        -> classOf[String]
  )
}

abstract class PicklerRuntime(classLoader: ClassLoader, preclazz: Class[_]) {

  val clazz = Runtime.toUnboxed.getOrElse(preclazz, preclazz)
  val mirror = runtimeMirror(classLoader)
  val sym = mirror.classSymbol(clazz)
  val tpe = sym.toType
  debug("PicklerRuntime: tpe = " + tpe)
  val irs = new IRs[ru.type](ru)
  import irs._
  val cir = classIR(tpe)
  debug("PicklerRuntime: cir = " + cir)

  def genPickler(implicit format: PickleFormat): Pickler[_]
}

class CompiledPicklerRuntime(classLoader: ClassLoader, clazz: Class[_]) extends PicklerRuntime(classLoader, clazz) {
  override def genPickler(implicit format: PickleFormat): Pickler[_] = {
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

class InterpretedPicklerRuntime(classLoader: ClassLoader, preclazz: Class[_]) extends PicklerRuntime(classLoader, preclazz) {

  debug("InterpretedPicklerRuntime: preclazz = " + preclazz)
  debug("InterpretedPicklerRuntime: clazz    = " + clazz)

  override def genPickler(implicit pf: PickleFormat): Pickler[_] = {
    // build "interpreted" runtime pickler
    new Pickler[Any] {
      val format = pf

      def pickle(picklee: Any, builder: PickleBuilder): Unit = {
        if (picklee != null) {
          val tag = TypeTag(tpe)
          if (format.isPrimitive(tpe)) {
            builder.beginEntryNoType(tag, picklee)
          } else {
            builder.beginEntry(tag, picklee)

            val im = mirror.reflect(picklee)

            cir.fields.foreach(fir => {
              if (fir.hasGetter) {
                val fldMirror   = im.reflectField(fir.field.get)
                val fldValue    = fldMirror.get
                debug("pickling field value: " + fldValue)
                val fldClass    = if (fldValue != null) fldValue.getClass else mirror.runtimeClass(NullTpe)
                // by using only the class we convert Int to Integer
                // therefore we pass fir.tpe (as pretpe) in addition to the class and use it for the is primitive check
                val fldRuntime = new InterpretedPicklerRuntime(classLoader, fldClass)
                val fldPickler = fldRuntime.genPickler.asInstanceOf[Pickler[Any]]
                builder.putField(fir.name, b => fldPickler.pickle(fldValue, b))
              }
            })
          }
          builder.endEntry()
        } else {
          builder.beginEntry(TypeTag(NullTpe), null)
          builder.endEntry()
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

class InterpretedUnpicklerRuntime(mirror: Mirror, tag: TypeTag[_]) {
  val tpe = tag.tpe
  val sym = tpe.typeSymbol.asType
  debug("UnpicklerRuntime: tpe = " + tpe)
  val clazz = mirror.runtimeClass(tpe)
  val irs = new IRs[ru.type](ru)
  import irs._
  val cir = classIR(tpe)
  debug("UnpicklerRuntime: cir = " + cir)

  def genUnpickler(implicit pf: PickleFormat, p1: Pickler[Int], p2: Pickler[String]): Unpickler[Any] = {
    new Unpickler[Any] {
      val format: PickleFormat = pf
      def unpickle(tag: TypeTag[_], reader: PickleReader): Any = {
        val pendingFields = cir.fields.filter(fir => fir.isNonParam || fir.isReifiedParam)
        val fieldVals = pendingFields.map(fir => {
          val freader = reader.readField(fir.name)
          if (format.isPrimitive(fir.tpe.erasure)) {
            val ftag = TypeTag(fir.tpe)
            freader.readPrimitive(ftag)
          } else {
            val fieldTag = freader.readTag(mirror)
            val fieldRuntime = new InterpretedUnpicklerRuntime(mirror, fieldTag)
            val fieldUnpickler = fieldRuntime.genUnpickler
            fieldUnpickler.unpickle(fieldTag, freader)
          }
        })

        val inst = scala.concurrent.util.Unsafe.instance.allocateInstance(clazz)
        val im = mirror.reflect(inst)

        pendingFields.zip(fieldVals) foreach {
          case (fir, fval) =>
            val fmX = im.reflectField(fir.field.get)
            fmX.set(fval)
        }

        inst
      }
    }
  }
}
