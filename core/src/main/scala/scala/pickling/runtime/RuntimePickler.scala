package scala.pickling
package runtime

import java.lang.reflect.Field

import scala.reflect.runtime.{universe => ru}
import ir._

import internal._


class RuntimeTypeInfo(classLoader: ClassLoader, clazz: Class[_], share: refs.Share) {
  import ru._
  import definitions._

  // debug(s"Initializing runtime type info for class '${clazz.getName}'...")

  val mirror: ru.Mirror =
    ru.runtimeMirror(classLoader)

  val sym =
    if (clazz != null) mirror.classSymbol(clazz) else NullClass
  // debug(s"sym: $sym")

  val tpe = {
    val elemClass: Class[_] =
      if (clazz != null) clazz.getComponentType() else null

    if (elemClass != null) // assume it's an array
      appliedType(ArrayClass.toType, List(mirror.classSymbol(elemClass).asType.toType))
    else
      sym.asType.toType
  }
  // debug(s"tpe: ${tpe.key}")

  val irs = new IRs[ru.type](ru)
  val cir = irs.newClassIR(tpe)
  // debug(s"CIR: ${cir.fields.mkString(",")}")

  // in runtime land, always use raw tags
  val tag = FastTypeTag.makeRaw(clazz)
  // debug(s"tag: $tag")

  val shareAnalyzer = new ShareAnalyzer[ru.type](ru) {
    def shareEverything = share.isInstanceOf[refs.ShareEverything]
    def shareNothing = share.isInstanceOf[refs.ShareNothing]
  }
  def shouldBotherAboutSharing(tpe: Type) = shareAnalyzer.shouldBotherAboutSharing(tpe)
  def shouldBotherAboutLooping(tpe: Type) = shareAnalyzer.shouldBotherAboutLooping(tpe)
}

// Note: This entire class must be constructed inside of a GRL-locked method.
class RuntimePickler(classLoader: ClassLoader, clazz: Class[_], fastTag: FastTypeTag[_])(implicit share: refs.Share) extends RuntimeTypeInfo(classLoader, clazz, share) {
  import ru._

  assert(scala.pickling.internal.GRL.isHeldByCurrentThread, "Failed to aquire GRL lock before instantiating a runtime pickler!")

  sealed abstract class Logic(fir: irs.FieldIR, isEffFinal: Boolean) {
    // debug(s"creating Logic for ${fir.name}")
    def run(builder: PBuilder, picklee: Any, im: ru.InstanceMirror): Unit = {
      val fldValue: Any = if (fir.accessor.nonEmpty) {
        val getterMirror = im.reflectMethod(fir.accessor.get)
        getterMirror()
      } else {
        val fldMirror = im.reflectField(fir.field.get)
        fldMirror.get
      }
      val fldClass = if (fldValue != null) fldValue.getClass else null

      //debug(s"pickling field of type: ${fir.tpe.toString}")
      //debug(s"isEffFinal: $isEffFinal")
      //debug(s"field value: $fldValue")
      //debug(s"field class: ${fldClass.getName}")

      // idea: picklers for all fields could be created and cached once and for all.
      // however, it depends on whether the type of the field is effectively final or not.
      // essentially, we have to emulate the behavior of generated picklers, which make
      // the same decision.
      // println(s"creating runtime pickler to pickle $fldClass field of class ${picklee.getClass.getName}")
      val fldTag = FastTypeTag.makeRaw(fldClass)
      // debug(s"!!! finding pickler for field with class ${fldClass.getName}")
      val fldPickler = scala.pickling.internal.currentRuntime.picklers.genPickler(classLoader, fldClass, fldTag).asInstanceOf[Pickler[Any]]
      //debug(s"looked up field pickler: $fldPickler")

      builder.putField(fir.name, b => {
        pickleLogic(fldClass, fldValue, b, fldPickler, fldTag)
      })
    }

    def pickleLogic(fieldClass: Class[_], fieldValue: Any, builder: PBuilder, pickler: Pickler[Any], fieldTag: FastTypeTag[_]): Unit
  }

  final class DefaultLogic(fir: irs.FieldIR) extends Logic(fir, false) {
    // debug(s"creating DefaultLogic for ${fir.name}")
    val staticClass = mirror.runtimeClass(fir.tpe.erasure)
    def pickleLogic(fldClass: Class[_], fldValue: Any, b: PBuilder, fldPickler: Pickler[Any], fldTag: FastTypeTag[_]): Unit = {
      pickleInto(fldClass, fldValue, b, fldPickler, fldTag)
    }
  }

  final class EffectivelyFinalLogic(fir: irs.FieldIR) extends Logic(fir, true) {
    // debug(s"creating EffectivelyFinalLogic for ${fir.name}")
    def pickleLogic(fldClass: Class[_], fldValue: Any, b: PBuilder, fldPickler: Pickler[Any], fldTag: FastTypeTag[_]): Unit = {
      b.hintElidedType(fldTag)
      pickleInto(fldClass, fldValue, b, fldPickler, fldTag)
    }
  }

  final class AbstractLogic(fir: irs.FieldIR) extends Logic(fir, false) {
    // debug(s"creating AbstractLogic for ${fir.name}")
    def pickleLogic(fldClass: Class[_], fldValue: Any, b: PBuilder, fldPickler: Pickler[Any], fldTag: FastTypeTag[_]): Unit = {
      pickleInto(fldClass, fldValue, b, fldPickler, fldTag)
    }
  }

  sealed class PrivateJavaFieldLogic(fir: irs.FieldIR, field: Field) extends Logic(fir, false) {
    // debug(s"creating PrivateJavaFieldLogic for ${fir.name}")
    override def run(builder: PBuilder, picklee: Any, im: ru.InstanceMirror): Unit = {
      field.setAccessible(true)
      val fldValue = field.get(picklee)
      val fldClass = if (fldValue != null) fldValue.getClass else null

      //debug(s"pickling field of type: ${fir.tpe.toString}")
      //debug(s"isEffFinal: $isEffFinal")
      //debug(s"field value: $fldValue")
      //debug(s"field class: ${fldClass.getName}")

      // idea: picklers for all fields could be created and cached once and for all.
      // however, it depends on whether the type of the field is effectively final or not.
      // essentially, we have to emulate the behavior of generated picklers, which make
      // the same decision.
      // debug(s"creating tag for field of class ${fldClass.getName}")
      val fldTag = FastTypeTag.makeRaw(fldClass)
      val fldPickler = scala.pickling.internal.currentRuntime.picklers.genPickler(classLoader, fldClass, fldTag).asInstanceOf[Pickler[Any]]
      // debug(s"looked up field pickler: $fldPickler")

      builder.putField(field.getName, b => {
        pickleLogic(fldClass, fldValue, b, fldPickler, fldTag)
      })
    }

    def pickleLogic(fldClass: Class[_], fldValue: Any, b: PBuilder, fldPickler: Pickler[Any], fldTag: FastTypeTag[_]): Unit = {
      pickleInto(fldClass, fldValue, b, fldPickler, fldTag)
    }
  }

  final class PrivateEffectivelyFinalJavaFieldLogic(fir: irs.FieldIR, field: Field) extends PrivateJavaFieldLogic(fir, field) {
    // debug(s"creating PrivateEffectivelyFinalJavaFieldLogic for ${fir.name}")
    override def pickleLogic(fldClass: Class[_], fldValue: Any, b: PBuilder, fldPickler: Pickler[Any], fldTag: FastTypeTag[_]): Unit = {
      b.hintElidedType(fldTag)
      pickleInto(fldClass, fldValue, b, fldPickler, fldTag)
    }
  }

  // difference to old runtime pickler: create tag based on fieldClass instead of fir.tpe
  def pickleInto(fieldClass: Class[_], fieldValue: Any, builder: PBuilder, pickler: Pickler[Any], fieldTag: FastTypeTag[_]): Unit = {
    //debug(s"fieldTag for pickleInto: ${fieldTag.key}")

    val fieldTpe = fieldTag.reflectType(mirror)
    if (shouldBotherAboutSharing(fieldTpe))
      fieldValue match {
        case null => pickler.asInstanceOf[Pickler[Null]].pickle(null, builder)
        case _ =>
          val oid = scala.pickling.internal.lookupPicklee(fieldValue)
          builder.hintOid(oid)
          // Note: Now we always pickle fully, and the format decides whether to share.
          pickler.pickle(fieldValue, builder)
      }
    else
      pickler.pickle(fieldValue, builder)
  }

  def mkPickler: Pickler[_] = {
    new Pickler[Any] {
      val fields: List[Logic] = cir.fields.flatMap { fir =>
        if (fir.accessor.nonEmpty)
          List(
            if (fir.tpe.typeSymbol.isEffectivelyFinal) new EffectivelyFinalLogic(fir)
            else if (fir.tpe.typeSymbol.asType.isAbstractType) new AbstractLogic(fir)
            else new DefaultLogic(fir)
          )
        else
          try {
            val javaField = clazz.getDeclaredField(fir.name)
            List(
              if (fir.tpe.typeSymbol.isEffectivelyFinal) new PrivateEffectivelyFinalJavaFieldLogic(fir, javaField)
              else new PrivateJavaFieldLogic(fir, javaField)
            )
          } catch {
            case e: java.lang.NoSuchFieldException => List()
          }
      }

      def putFields(picklee: Any, builder: PBuilder): Unit = {
        val im = mirror.reflect(picklee)
        fields.foreach(_.run(builder, picklee, im))
      }

      def tag: FastTypeTag[Any] = fastTag.asInstanceOf[FastTypeTag[Any]]

      def pickle(picklee: Any, builder: PBuilder): Unit = {
        scala.pickling.internal.GRL.lock()
        //debug(s"pickling object of type: ${tag.key}")
        try {
          builder.beginEntry(picklee, tag)
          putFields(picklee, builder)
          builder.endEntry()
        } finally scala.pickling.internal.GRL.unlock()
      }
    }
  }

}
