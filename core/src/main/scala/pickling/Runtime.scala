package scala.pickling

import scala.reflect.runtime.universe._
import definitions._
import scala.reflect.runtime.{universe => ru}
import ir._
import scala.tools.reflect.ToolBox

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

  val clazz = if (preclazz != null) Runtime.toUnboxed.getOrElse(preclazz, preclazz) else null
  val mirror = runtimeMirror(classLoader)
  val sym = if (clazz != null) mirror.classSymbol(clazz) else NullClass
  val tpe = {
    // TODO: fix duplication w.r.t Tools.scala
    val tpeWithMaybeTparams = sym.asType.toType
    val tparams = tpeWithMaybeTparams match {
      case TypeRef(_, _, targs) => targs.map(_.typeSymbol)
      case _ => Nil
    }
    existentialAbstraction(tparams, tpeWithMaybeTparams)
  }
  val tag = FastTypeTag(mirror, tpe, tpe.key)
  debug("PicklerRuntime: tpe = " + tpe)
  val irs = new IRs[ru.type](ru)
  import irs._
  val cir = flattenedClassIR(tpe)
  debug("PicklerRuntime: cir = " + cir)

  def genPickler(implicit format: PickleFormat): SPickler[_]
}

class InterpretedPicklerRuntime(classLoader: ClassLoader, preclazz: Class[_]) extends PicklerRuntime(classLoader, preclazz) {

  debug("InterpretedPicklerRuntime: preclazz = " + preclazz)
  debug("InterpretedPicklerRuntime: clazz    = " + clazz)

  override def genPickler(implicit pf: PickleFormat): SPickler[_] = {
    // build "interpreted" runtime pickler
    new SPickler[Any] with PickleTools {
      val format: PickleFormat = pf
      def pickle(picklee: Any, builder: PBuilder): Unit = {
        if (picklee != null) {
          builder.hintTag(tag)
          builder.beginEntry(picklee)

          // TODO: need to support modules and other special guys here
          lazy val im = mirror.reflect(picklee)
          cir.fields.filter(_.hasGetter).foreach(fir => {
            val fldMirror = im.reflectField(fir.field.get)
            val fldValue = fldMirror.get
            debug("pickling field value: " + fldValue)
            val fldClass = if (fldValue != null) fldValue.getClass else null
            // by using only the class we convert Int to Integer
            // therefore we pass fir.tpe (as pretpe) in addition to the class and use it for the is primitive check
            val fldRuntime = new InterpretedPicklerRuntime(classLoader, fldClass)
            val fldPickler = fldRuntime.genPickler.asInstanceOf[SPickler[Any]]
            builder.putField(fir.name, b => {
              val fstaticTpe = fir.tpe.erasure
              if (fldClass == null || fldClass == mirror.runtimeClass(fstaticTpe)) builder.hintDynamicallyElidedType()
              if (fstaticTpe.typeSymbol.isEffectivelyFinal) builder.hintStaticallyElidedType()
              fldPickler.pickle(fldValue, b)
            })
          })

          builder.endEntry()
        } else {
          builder.hintTag(FastTypeTag.Null)
          builder.beginEntry(null)
          builder.endEntry()
        }
      }
    }
  }
}

class CompiledPicklerRuntime(classLoader: ClassLoader, clazz: Class[_]) extends PicklerRuntime(classLoader, clazz) {
  override def genPickler(implicit format: PickleFormat): SPickler[_] = {
    // TODO: we should somehow cache toolboxes. maybe even inside the reflection API
    // TODO: toolbox bug. if we don't explicitly import PickleOps, it will fail to be found
    // more precisely: it will be found, but then immediately discarded, because a reference to it won't typecheck
    val formatTpe = mirror.reflect(format).symbol.asType.toType
    mirror.mkToolBox().eval(q"""
      import scala.pickling._
      import scala.pickling.`package`.PickleOps
      implicit val format: $formatTpe = new $formatTpe()
      implicitly[SPickler[$tpe]]
    """).asInstanceOf[SPickler[_]]
  }
}

class InterpretedUnpicklerRuntime(mirror: Mirror, tag: FastTypeTag[_]) {
  val tpe = tag.tpe
  val sym = tpe.typeSymbol.asType
  debug("UnpicklerRuntime: tpe = " + tpe)
  val clazz = mirror.runtimeClass(tpe.erasure)
  val irs = new IRs[ru.type](ru)
  import irs._
  val cir = flattenedClassIR(tpe)
  debug("UnpicklerRuntime: cir = " + cir)

  def genUnpickler(implicit pf: PickleFormat): Unpickler[Any] = {
    new Unpickler[Any] with PickleTools {
      val format: PickleFormat = pf
      def unpickle(tag: => FastTypeTag[_], reader: PReader): Any = {
        if (reader.atPrimitive) reader.readPrimitive()
        else {
          // TODO: need to support modules and other special guys here
          val pendingFields = cir.fields.filter(fir => fir.isNonParam || fir.isReifiedParam)
          val fieldVals = pendingFields.map(fir => {
            val freader = reader.readField(fir.name)
            val fstaticTag = FastTypeTag(mirror, fir.tpe, fir.tpe.key)
            freader.hintTag(fstaticTag)

            val fstaticSym = fstaticTag.tpe.typeSymbol
            if (fstaticSym.isEffectivelyFinal) freader.hintStaticallyElidedType()
            val fdynamicTag = freader.beginEntry()

            val fval = {
              if (freader.atPrimitive) freader.readPrimitive()
              else {
                val fieldRuntime = new InterpretedUnpicklerRuntime(mirror, fdynamicTag)
                val fieldUnpickler = fieldRuntime.genUnpickler
                fieldUnpickler.unpickle(fdynamicTag, freader)
              }
            }

            freader.endEntry()
            fval
          })

          val inst = scala.concurrent.util.Unsafe.instance.allocateInstance(clazz)
          val im = mirror.reflect(inst)

          pendingFields.zip(fieldVals) foreach {
            case (fir, fval) =>
              val fmX = im.reflectField(fir.field.get)
              fmX.forcefulSet(fval)
          }

          inst
        }
      }
    }
  }
}

// TODO: copy/paste wrt CompiledPicklerRuntime
class CompiledUnpicklerRuntime(mirror: Mirror, tpe: Type) {
  def genUnpickler(implicit format: PickleFormat): Unpickler[_] = {
    // see notes and todos in CompiledPicklerRuntime.genPickler
    val formatTpe = mirror.reflect(format).symbol.asType.toType
    mirror.mkToolBox().eval(q"""
      import scala.pickling._
      import scala.pickling.`package`.PickleOps
      implicit val format: $formatTpe = new $formatTpe()
      implicitly[Unpickler[$tpe]]
    """).asInstanceOf[Unpickler[_]]
  }
}
