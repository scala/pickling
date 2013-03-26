package scala.pickling

import scala.reflect.runtime.universe._
import language.experimental.macros

trait CorePicklersUnpicklers extends GenPicklers with GenUnpicklers {
  class PrimitivePicklerUnpickler[T: TypeTag](implicit val format: PickleFormat) extends Pickler[T] with Unpickler[T] {
    def pickle(picklee: T, builder: PickleBuilder): Unit = {
      if (picklee != null) builder.hintTag(typeTag[T])
      else builder.hintTag(typeTag[Null])
      builder.beginEntry(picklee)
      builder.endEntry()
    }
    def unpickle(tag: TypeTag[_], reader: PickleReader): Any = {
      // NOTE: here we essentially require that ints and strings are primitives for all readers
      // TODO: discuss that and see whether it defeats all the purpose of abstracting primitives away from picklers
      if (tag.key != typeTag[Null].key) reader.readPrimitive()
      else null
    }
  }

  implicit def intPicklerUnpickler(implicit format: PickleFormat): PrimitivePicklerUnpickler[Int] = new PrimitivePicklerUnpickler[Int]
  implicit def stringPicklerUnpickler(implicit format: PickleFormat): PrimitivePicklerUnpickler[String] = new PrimitivePicklerUnpickler[String]
  implicit def booleanPicklerUnpickler(implicit format: PickleFormat): PrimitivePicklerUnpickler[Boolean] = new PrimitivePicklerUnpickler[Boolean]
  implicit def nullPicklerUnpickler(implicit format: PickleFormat): PrimitivePicklerUnpickler[Null] = new PrimitivePicklerUnpickler[Null]
  implicit def genArrayPickler[T](implicit format: PickleFormat): Pickler[Array[T]] with Unpickler[Array[T]] = macro ArrayPicklerUnpicklerMacro.impl[T]
  // TODO: if you uncomment this one, it will shadow picklers/unpicklers for Int and String. why?!
  // TODO: due to the inability to implement module pickling/unpickling in a separate macro, I moved the logic into genPickler/genUnpickler
  // implicit def modulePicklerUnpickler[T <: Singleton](implicit format: PickleFormat): Pickler[T] with Unpickler[T] = macro ModulePicklerUnpicklerMacro.impl[T]
}

trait ArrayPicklerUnpicklerMacro extends Macro {
  def impl[T: c.WeakTypeTag](format: c.Tree): c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[Array[T]]
    val eltpe = weakTypeOf[T]
    val isPrimitive = eltpe.typeSymbol.asClass.isPrimitive
    val picklerUnpickler = {
      c.topLevelRef(syntheticPicklerUnpicklerQualifiedName(tpe)) orElse c.introduceTopLevel(syntheticPackageName, {
        q"""
          class ${syntheticPicklerUnpicklerName(tpe)} extends scala.pickling.Pickler[$tpe] with scala.pickling.Unpickler[$tpe] {
            import scala.reflect.runtime.universe._
            import scala.pickling._
            import scala.pickling.`package`.PickleOps
            import scala.collection.mutable.ArrayBuffer
            implicit val format = new ${format.tpe}()
            def pickle(picklee: $tpe, builder: PickleBuilder): Unit = {
              if (!$isPrimitive) throw new PicklingException(s"implementation restriction: non-primitive arrays aren't supported")
              builder.hintTag(scala.reflect.runtime.universe.typeTag[$tpe]).beginEntry()
              builder.beginCollection(picklee.length)
              var i = 0
              while (i < picklee.length) {
                val el = picklee(i)
                builder.putElement(b => el.pickleInto(b.hintStaticallyElidedType()))
                i += 1
              }
              builder.endCollection()
              builder.endEntry()
            }
            def unpickle(tag: TypeTag[_], reader: PickleReader): Any = {
              if (!$isPrimitive) throw new PicklingException(s"implementation restriction: non-primitive arrays aren't supported")
              var buf = ArrayBuffer[$eltpe]()
              val arrReader = reader.beginCollection()
              val length = arrReader.readLength()
              var i = 0
              while (i < length) {
                val el = arrReader.readElement().unpickle[$eltpe]
                buf += el
                i += 1
              }
              arrReader.endCollection()
              buf.toArray
            }
          }
        """
      })
    }
    q"new $picklerUnpickler"
  }
}

trait ModulePicklerUnpicklerMacro extends Macro {
  def impl[T: c.WeakTypeTag](format: c.Tree): c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val module = tpe.typeSymbol.asClass.module
    if (module == NoSymbol) c.diverge()
    val picklerUnpickler = {
      c.topLevelRef(syntheticPicklerUnpicklerQualifiedName(tpe)) orElse c.introduceTopLevel(syntheticPackageName, {
        q"""
          class ${syntheticPicklerUnpicklerName(tpe)} extends scala.pickling.Pickler[$tpe] with scala.pickling.Unpickler[$tpe] {
            import scala.reflect.runtime.universe._
            import scala.pickling._
            import scala.pickling.`package`.PickleOps
            implicit val format = new ${format.tpe}()
            def pickle(picklee: $tpe, builder: PickleBuilder): Unit = {
              builder.hintTag(scala.reflect.runtime.universe.typeTag[$tpe])
              builder.beginEntry(picklee)
              builder.endEntry()
            }
            def unpickle(tag: TypeTag[_], reader: PickleReader): Any = {
              $module
            }
          }
        """
      })
    }
    q"new $picklerUnpickler"
  }
}
