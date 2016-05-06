package scala.pickling
package runtime

import scala.pickling.PicklingErrors.BasePicklingException
import scala.pickling.internal._

import scala.reflect.runtime.universe.Mirror
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

import HasCompat._
abstract class PicklerRuntime(classLoader: ClassLoader, preclazz: Class[_], share: refs.Share) {
  import scala.reflect.runtime.universe._
  import definitions._
  import scala.reflect.runtime.{universe => ru}
  import compat._

  val clazz = if (preclazz != null) Runtime.toUnboxed.getOrElse(preclazz, preclazz) else null
  val mirror = runtimeMirror(classLoader)
  val sym = if (clazz != null) mirror.classSymbol(clazz) else NullClass
  val tpe = {
    val elType = if (clazz != null) clazz.getComponentType() else null
    if (elType != null) {
      // TODO: correctly convert elType
      appliedType(ArrayClass.toType, List(mirror.classSymbol(elType).asType.toType))
    } else {
      // TODO: fix duplication w.r.t Tools.scala
      val tpeWithMaybeTparams = sym.asType.toType
      val tparams = tpeWithMaybeTparams match {
        case TypeRef(_, _, targs) => targs.map(_.typeSymbol)
        case _ => Nil
      }
      existentialAbstraction(tparams, tpeWithMaybeTparams)
    }
  }
  val tag = FastTypeTag.makeRaw(clazz)
  //debug(s"PicklerRuntime: tpe = $tpe, tag = ${tag.toString}")
  val irs = new IRs[ru.type](ru)
  import irs._
  val cir = newClassIR(tpe)
  //debug(s"PicklerRuntime: cir = $cir")

  val shareAnalyzer = new ShareAnalyzer[ru.type](ru) {
    def shareEverything = share.isInstanceOf[refs.ShareEverything]
    def shareNothing = share.isInstanceOf[refs.ShareNothing]
  }
  def shouldBotherAboutSharing(tpe: Type) = shareAnalyzer.shouldBotherAboutSharing(tpe)
  def shouldBotherAboutLooping(tpe: Type) = shareAnalyzer.shouldBotherAboutLooping(tpe)
}

class InterpretedPicklerRuntime(classLoader: ClassLoader, preclazz: Class[_])(implicit share: refs.Share) extends PicklerRuntime(classLoader, preclazz, share) {
  import scala.reflect.runtime.universe._

  debug("InterpretedPicklerRuntime: preclazz = " + preclazz)
  debug("InterpretedPicklerRuntime: clazz    = " + clazz)

  //  TODO - this pickler should know to lock the GRL before running itself, or any mirror code.
  def genPickler: Pickler[_] = {
    // build "interpreted" runtime pickler
    new Pickler[Any] with PickleTools {
      val fields: List[(irs.FieldIR, Boolean)] =
        cir.fields.filter(_.hasGetter).map(fir => (fir, fir.tpe.typeSymbol.isEffectivelyFinal))

      def tag: FastTypeTag[Any] = FastTypeTag.makeRaw(clazz).asInstanceOf[FastTypeTag[Any]]

      def pickleInto(fieldTpe: Type, picklee: Any, builder: PBuilder, pickler: Pickler[Any]): Unit = {
        if (shouldBotherAboutSharing(fieldTpe))
          picklee match {
            case null => pickler.asInstanceOf[Pickler[Null]].pickle(null, builder)
            case _ =>
              val oid = scala.pickling.internal.lookupPicklee(picklee)
              builder.hintOid(oid)
              pickler.pickle(picklee, builder)
          }
        else
          pickler.pickle(picklee, builder)
      }

      def pickle(picklee: Any, builder: PBuilder): Unit = {
        if (picklee != null) {
           def putFields() = {
            // TODO: need to support modules and other special guys here
            lazy val im = mirror.reflect(picklee)
            fields.foreach { case (fir, isEffFinal) =>
              val fldMirror = im.reflectField(fir.field.get)
              val fldValue: Any = fldMirror.get
              // debug("pickling field value: " + fldValue)

              val fldClass = if (fldValue != null) fldValue.getClass else null
              // by using only the class we convert Int to Integer
              // therefore we pass fir.tpe (as pretpe) in addition to the class and use it for the is primitive check
              //val fldRuntime = new InterpretedPicklerRuntime(classLoader, fldClass)
              val fldTag = FastTypeTag.makeRaw(fldClass)
              val fldPickler = scala.pickling.internal.currentRuntime.picklers.genPickler(classLoader, fldClass, fldTag).asInstanceOf[Pickler[Any]]

              builder.putField(fir.name, b => {
                if (isEffFinal) {
                  b.hintElidedType(fldTag)
                  pickleInto(fir.tpe, fldValue, b, fldPickler)
                } else  {
                  val subPicklee = fldValue
                  if (subPicklee == null || subPicklee.getClass == mirror.runtimeClass(fir.tpe.erasure)) b.hintElidedType(fldTag) else ()
                  pickleInto(fir.tpe, subPicklee, b, fldPickler)
                }
              })

              // builder.putField(fir.name, b => {
              //   val fstaticTpe = fir.tpe.erasure
              //   if (fldClass == null || fldClass == mirror.runtimeClass(fstaticTpe)) builder.hintDynamicallyElidedType()
              //   if (fstaticTpe.typeSymbol.isEffectivelyFinal) builder.hintStaticallyElidedType()
              //   fldPickler.pickle(fldValue, b)
              // })
            }
          }
          builder.beginEntry(picklee, tag)
          GRL.lock()
          try putFields()
          finally GRL.unlock()
          builder.endEntry()
        } else {
          builder.beginEntry(null, FastTypeTag.Null)
          builder.endEntry()
        }
      }
    }
  }
}

trait UnpicklerRuntime {
  def genUnpickler: Unpickler[Any]
  def mirror: Mirror
  import scala.reflect.runtime.{universe => ru}
  def classForType(t: ru.Type): Class[_] =
    if (t =:= ru.typeOf[Any]) classOf[Any]
    else mirror.runtimeClass(t)
}

// TODO: currently this works with an assumption that sharing settings for unpickling are the same as for pickling
// of course this might not be the case, so we should be able to read `share` from the pickle itself
class InterpretedUnpicklerRuntime(val mirror: Mirror, typeTag: String)(implicit share: refs.Share)
    extends UnpicklerRuntime {
  import scala.reflect.runtime.universe._
  import definitions._
  import scala.reflect.runtime.{universe => ru}

  val fastTag = FastTypeTag(typeTag)
  val tpe = fastTag.reflectType(mirror)
  val sym = tpe.typeSymbol.asType
  // debug("UnpicklerRuntime: tpe = " + tpe)
  val clazz = mirror.runtimeClass(tpe.erasure)
  val irs = new IRs[ru.type](ru)
  import irs._
  val cir = newClassIR(tpe)
  // debug("UnpicklerRuntime: cir = " + cir)

  val shareAnalyzer = new ShareAnalyzer[ru.type](ru) {
    def shareEverything = share.isInstanceOf[refs.ShareEverything]
    def shareNothing = share.isInstanceOf[refs.ShareNothing]
  }
  def shouldBotherAboutSharing(tpe: Type) = shareAnalyzer.shouldBotherAboutSharing(tpe)
  def shouldBotherAboutLooping(tpe: Type) = shareAnalyzer.shouldBotherAboutLooping(tpe)

  // TODO - This method should lock the GRL before running any unpickle logic.
  def genUnpickler: Unpickler[Any] = {
    new Unpickler[Any] with PickleTools {
      def tag: FastTypeTag[Any] = fastTag.asInstanceOf[FastTypeTag[Any]]
      def unpickle(tagKey: String, reader: PReader): Any = {
        scala.pickling.internal.GRL.lock()
        try {
          if (cir.javaGetInstance) {
            clazz.getDeclaredMethod("getInstance").invoke(null)
          } else if (reader.atPrimitive) {
            val result = reader.readPrimitive()
            if (shouldBotherAboutSharing(tpe)) registerUnpicklee(result, preregisterUnpicklee())
            result
          } else if (tagKey.endsWith("$")) {
            val c = Class.forName(tagKey)
            c.getField("MODULE$").get(c)
          } else {
            val pendingFields =
              if (tagKey.contains("anonfun$")) List[FieldIR]()
              else cir.fields.filter(fir =>
                fir.hasGetter || {
                  // exists as Java field
                  scala.util.Try(clazz.getDeclaredField(fir.name)).isSuccess
                })

            def fieldVals = pendingFields.map(fir => {
              val freader = reader.readField(fir.name)
              val fstaticTag = FastTypeTag.makeRaw(classForType(fir.tpe))
              val fstaticSym = fstaticTag.reflectType(mirror).typeSymbol
              if (fstaticSym.isEffectivelyFinal) freader.hintElidedType(fstaticTag)
              val fdynamicTag = try {
                freader.beginEntry()
              } catch {
                case e@BasePicklingException(msg, cause) =>
                  debug( s"""error in interpreted runtime unpickler while reading tag of field '${fir.name}':
                            |$msg
                            |enclosing object has type: '${tagKey}'
                            |static type of field: '${fir.tpe}'
                            |""".stripMargin)
                throw e
            }
            val
            fval = {
              if (freader.atPrimitive) {
                val result = freader.readPrimitive()
                if (shouldBotherAboutSharing(fir.tpe)) registerUnpicklee(result, preregisterUnpicklee())
                result
              } else {
                val fieldUnpickler = scala.pickling.internal.currentRuntime.picklers.genUnpickler(mirror, fdynamicTag)
                fieldUnpickler.unpickle(fdynamicTag, freader)
              }
            }

            freader.endEntry()
            fval
          })

          // TODO: need to support modules and other special guys here
          // TODO: in principle, we could invoke a constructor here
          val inst = scala.concurrent.util.Unsafe.instance.allocateInstance(clazz)
          if (shouldBotherAboutSharing(tpe)) registerUnpicklee(inst, preregisterUnpicklee())
          val im = mirror.reflect(inst)

          //debug(s"pendingFields: ${pendingFields.size}")
          //debug(s"fieldVals: ${fieldVals.size}")

          pendingFields.zip(fieldVals) foreach {
            case (fir, fval) =>
              if (fir.field.nonEmpty) {
                val fmX = im.reflectField(fir.field.get)
                fmX.set(fval)
              } else {
                val javaField = clazz.getDeclaredField(fir.name)
                javaField.setAccessible(true)
                javaField.set(inst, fval)
              }
          }

          inst
        }
        } finally GRL.unlock()
      }
    }
  }
}

class ShareNothingInterpretedUnpicklerRuntime(val mirror: Mirror, typeTag: String)(implicit share: refs.Share)
    extends UnpicklerRuntime {
  import scala.reflect.runtime.universe._
  import definitions._
  import scala.reflect.runtime.{universe => ru}

  val fastTag = FastTypeTag(typeTag)
  val tpe = fastTag.reflectType(mirror)
  val sym = tpe.typeSymbol.asType
  // debug("UnpicklerRuntime: tpe = " + tpe)
  val clazz = mirror.runtimeClass(tpe.erasure)
  val irs = new IRs[ru.type](ru)
  import irs._
  val cir = newClassIR(tpe)
  // debug("UnpicklerRuntime: cir = " + cir)

  // TODO - This method should lock the GRL before running any unpickle logic
  def genUnpickler: Unpickler[Any] = {
    new Unpickler[Any] with PickleTools {
      def tag: FastTypeTag[Any] = fastTag.asInstanceOf[FastTypeTag[Any]]
      def unpickle(tagKey: String, reader: PReader): Any = {
        GRL.lock()
        try {
        if (reader.atPrimitive) {
          reader.readPrimitive()
        } else if (tagKey.endsWith("$")) {
          val c = Class.forName(tagKey)
          c.getField("MODULE$").get(c)
        } else {
          val pendingFields =
            if (tagKey.contains("anonfun$")) {
              List[FieldIR]()
            } else {
              cir.fields.filter(fir =>
                fir.hasGetter || {
                  // exists as Java field
                  scala.util.Try(clazz.getDeclaredField(fir.name)).isSuccess
                })
            }

          def fieldVals = pendingFields.map(fir => {
            val freader = reader.readField(fir.name)
            val fstaticTag = FastTypeTag.makeRaw(classForType(fir.tpe))
            val fstaticSym = fstaticTag.reflectType(mirror).typeSymbol
            if (fstaticSym.isEffectivelyFinal) freader.hintElidedType(fstaticTag)
            val fdynamicTag = try {
              freader.beginEntry()
            } catch {
              case e @ BasePicklingException(msg, cause) =>
                debug(s"""error in interpreted runtime unpickler while reading tag of field '${fir.name}':
                         |$msg
                         |enclosing object has type: '${tagKey}'
                         |static type of field: '${fir.tpe}'
                         |""".stripMargin)
                throw e
            }
            val fval = {
              if (freader.atPrimitive) {
                val result = freader.readPrimitive()
                result
              } else {
                val fieldUnpickler = scala.pickling.internal.currentRuntime.picklers.genUnpickler(mirror, fdynamicTag)
                fieldUnpickler.unpickle(fdynamicTag, freader)
              }
            }

            freader.endEntry()
            fval
          })

          // TODO: need to support modules and other special guys here
          // TODO: in principle, we could invoke a constructor here
          val inst = scala.concurrent.util.Unsafe.instance.allocateInstance(clazz)
          val im = mirror.reflect(inst)

          //debug(s"pendingFields: ${pendingFields.size}")
          //debug(s"fieldVals: ${fieldVals.size}")

          pendingFields.zip(fieldVals) foreach {
            case (fir, fval) =>
              if (fir.field.nonEmpty) {
                val fmX = im.reflectField(fir.field.get)
                fmX.set(fval)
              } else {
                val javaField = clazz.getDeclaredField(fir.name)
                javaField.setAccessible(true)
                javaField.set(inst, fval)
              }
          }

          inst
        }
        } finally GRL.unlock()
      }
    }
  }
}
