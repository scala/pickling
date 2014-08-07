package scala.pickling

import scala.reflect.runtime.{universe => ru}
import ir._

import internal._


class RuntimeTypeInfo(classLoader: ClassLoader, clazz: Class[_]) {
  import ru._
  import definitions._

  val mirror: ru.Mirror =
    ru.runtimeMirror(classLoader)

  val sym =
    if (clazz != null) mirror.classSymbol(clazz) else NullClass

  val tpe = {
    val elemClass: Class[_] =
      if (clazz != null) clazz.getComponentType() else null

    if (elemClass != null) // assume it's an array
      appliedType(ArrayClass.toType, List(mirror.classSymbol(elemClass).asType.toType))
    else
      sym.asType.toType
  }

  val irs = new IRs[ru.type](ru)
  val cir = irs.flattenedClassIR(tpe)

  // this might not create the right tag if tpe is computed incorrectly
  val tag = FastTypeTag(mirror, tpe, /*tpe.key*/clazz.getName)
}

// TODO: sharing
class RuntimePickler(classLoader: ClassLoader, clazz: Class[_]) extends RuntimeTypeInfo(classLoader, clazz) {
  import ru._

  // difference to old runtime pickler: create tag based on fieldClass instead of fir.tpe
  def pickleInto(fieldClass: Class[_], fieldValue: Any, builder: PBuilder, pickler: SPickler[Any]): Unit = {
    val fieldTag = FastTypeTag.mkRaw(fieldClass, mirror)
    debug(s"fieldTag for pickleInto: ${fieldTag.key}")
    builder.hintTag(fieldTag)
    pickler.pickle(fieldValue, builder)
  }

  def mkPickler(implicit pf: PickleFormat): SPickler[_] = {
    //???
    new SPickler[Any] {
      val format: PickleFormat = pf

      val fields: List[(irs.FieldIR, Boolean)] =
        cir.fields.filter(_.hasGetter).map(fir => (fir, fir.tpe.typeSymbol.isEffectivelyFinal))

      def putFields(picklee: Any, builder: PBuilder): Unit = {
        val im = mirror.reflect(picklee)
        fields.foreach { case (fir, isEffFinal) =>
          val fldMirror = im.reflectField(fir.field.get)
          val fldValue: Any = fldMirror.get
          val fldClass = if (fldValue != null) fldValue.getClass else null

          debug(s"pickling field of type: ${fir.tpe.toString}")
          debug(s"isEffFinal: $isEffFinal")
          debug(s"field value: $fldValue")
          debug(s"field class: ${fldClass.getName}")

          // idea: picklers for all fields could be created and cached once and for all.
          // however, it depends on whether the type of the field is effectively final or not.
          // essentially, we have to emulate the behavior of generated picklers, which make
          // the same decision.
          val fldPickler = SPickler.genPickler(classLoader, fldClass).asInstanceOf[SPickler[Any]]
          debug(s"looked up field pickler: $fldPickler")

/* copied from Macros.scala:
builder.putField(${fir.name}, b => $pickleLogic)

if (fir.tpe.typeSymbol.isEffectivelyFinal) q"""
                b.hintStaticallyElidedType()
                $getterLogic.pickleInto(b)
              """ else q"""
                val subPicklee: ${fir.tpe} = $getterLogic
                if (subPicklee == null || subPicklee.getClass == classOf[${fir.tpe}]) b.hintDynamicallyElidedType() else ()
                subPicklee.pickleInto(b)
              """

val pickler = $dispatchLogic
$builder.hintTag(implicitly[scala.pickling.FastTypeTag[$tpe]])
pickler.asInstanceOf[SPickler[$tpe]].pickle(picklee, $builder)
*/

          builder.putField(fir.name, b => {
            if (isEffFinal) { // this has to correctly fields of type Int, e.g.
              b.hintStaticallyElidedType()
              pickleInto(fldClass, fldValue, b, fldPickler)
            } else  {
              val subPicklee = fldValue
              if (subPicklee == null || subPicklee.getClass == mirror.runtimeClass(fir.tpe.erasure)) b.hintDynamicallyElidedType() else ()
              pickleInto(fldClass, subPicklee, b, fldPickler)
            }
          })
        } // foreach
      }

      def pickle(picklee: Any, builder: PBuilder): Unit = {
        debug(s"pickling object of type: ${tag.key}")
        builder.hintTag(tag)
        builder.beginEntry(picklee)
        putFields(picklee, builder)
        builder.endEntry()
      }
    }
  }

}
