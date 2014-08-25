package scala.pickling

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

// provides a source compatibility stub
// in Scala 2.10.x, it will make `import compat._` compile just fine,
// even though `c.universe` doesn't have `compat`
// in Scala 2.11.0, it will be ignored, becase `import c.universe._`
// brings its own `compat` in scope and that one takes precedence
private object HasCompat { val compat = ??? }; import HasCompat._

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
  val tag = FastTypeTag(mirror, tpe, tpe.key)
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

  def genPickler(implicit format: PickleFormat): SPickler[_]
}

class InterpretedPicklerRuntime(classLoader: ClassLoader, preclazz: Class[_])(implicit share: refs.Share) extends PicklerRuntime(classLoader, preclazz, share) {
  import scala.reflect.runtime.universe._

  debug("InterpretedPicklerRuntime: preclazz = " + preclazz)
  debug("InterpretedPicklerRuntime: clazz    = " + clazz)

  override def genPickler(implicit pf: PickleFormat): SPickler[_] = {
    // build "interpreted" runtime pickler
    new SPickler[Any] with PickleTools {
      val format: PickleFormat = pf

      val fields: List[(irs.FieldIR, Boolean)] =
        cir.fields.filter(_.hasGetter).map(fir => (fir, fir.tpe.typeSymbol.isEffectivelyFinal))

      def pickleInto(fieldTpe: Type, picklee: Any, builder: PBuilder, pickler: SPickler[Any]): Unit = {
        if (shouldBotherAboutSharing(fieldTpe))
          picklee match {
            case null => pickler.asInstanceOf[SPickler[Null]].pickle(null, builder)
            case _ =>
              val oid = scala.pickling.internal.lookupPicklee(picklee)
              builder.hintOid(oid)
              if (oid == -1) {
                pickler.pickle(picklee, builder)
              } else {
                builder.beginEntry(picklee)
                builder.endEntry()
              }
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
              val fldTag = FastTypeTag.mkRaw(fldClass, mirror)
              val fldPickler = SPickler.genPickler(classLoader, fldClass, fldTag).asInstanceOf[SPickler[Any]]

              builder.putField(fir.name, b => {
                if (isEffFinal) {
                  b.hintStaticallyElidedType()
                  pickleInto(fir.tpe, fldValue, b, fldPickler)
                } else  {
                  val subPicklee = fldValue
                  if (subPicklee == null || subPicklee.getClass == mirror.runtimeClass(fir.tpe.erasure)) b.hintDynamicallyElidedType() else ()
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

          builder.hintTag(tag)
          builder.beginEntry(picklee)
          putFields()
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

// TODO: currently this works with an assumption that sharing settings for unpickling are the same as for pickling
// of course this might not be the case, so we should be able to read `share` from the pickle itself
class InterpretedUnpicklerRuntime(mirror: Mirror, tag: FastTypeTag[_])(implicit share: refs.Share) {
  import scala.reflect.runtime.universe._
  import definitions._
  import scala.reflect.runtime.{universe => ru}

  val tpe = tag.tpe
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

  def genUnpickler(implicit pf: PickleFormat): Unpickler[Any] = {
    new Unpickler[Any] with PickleTools {
      val format: PickleFormat = pf
      def unpickle(tag: => FastTypeTag[_], reader: PReader): Any = {
        if (reader.atPrimitive) {
          val result = reader.readPrimitive()
          if (shouldBotherAboutSharing(tpe)) registerUnpicklee(result, preregisterUnpicklee())
          result
        } else {
          val pendingFields =
            if (tag.key.contains("anonfun$")) {
              List[FieldIR]()
            } else {
              val (nonLoopyFields, loopyFields) = cir.fields.partition(fir => !shouldBotherAboutLooping(fir.tpe))
              (nonLoopyFields ++ loopyFields).filter(fir =>
                fir.hasGetter || {
                  // exists as Java field
                  scala.util.Try(clazz.getDeclaredField(fir.name)).isSuccess
                })
            }

          def fieldVals = pendingFields.map(fir => {
            val freader = reader.readField(fir.name)
            val fstaticTag = FastTypeTag(mirror, fir.tpe, fir.tpe.key)
            freader.hintTag(fstaticTag)

            val fstaticSym = fstaticTag.tpe.typeSymbol
            if (fstaticSym.isEffectivelyFinal) freader.hintStaticallyElidedType()
            val fdynamicTag = freader.beginEntry()

            val fval = {
              if (freader.atPrimitive) {
                val result = freader.readPrimitive()
                if (shouldBotherAboutSharing(fir.tpe)) registerUnpicklee(result, preregisterUnpicklee())
                result
              } else {
                val fieldRuntime = new InterpretedUnpicklerRuntime(mirror, fdynamicTag)
                val fieldUnpickler = fieldRuntime.genUnpickler
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
      }
    }
  }
}
