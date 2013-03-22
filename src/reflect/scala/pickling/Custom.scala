package scala.pickling

import scala.reflect.runtime.universe._
import language.experimental.macros

trait CorePicklersUnpicklers extends GenPicklers with GenUnpicklers {
  class PrimitivePicklerUnpickler[T: TypeTag](implicit val format: PickleFormat) extends Pickler[T] with Unpickler[T] {
    def pickle(picklee: Any, builder: PickleBuilder): Unit = {
      builder.beginEntryNoType(typeTag[T], picklee)
      builder.endEntry()
    }
    def unpickle(tag: TypeTag[_], reader: PickleReader): Any = {
      // NOTE: here we essentially require that ints and strings are primitives for all readers
      // TODO: discuss that and see whether it defeats all the purpose of abstracting primitives away from picklers
      reader.readPrimitive(typeTag[T])
    }
  }

  implicit def intPicklerUnpickler(implicit format: PickleFormat): PrimitivePicklerUnpickler[Int] = new PrimitivePicklerUnpickler[Int]()
  implicit def stringPicklerUnpickler(implicit format: PickleFormat): PrimitivePicklerUnpickler[String] = new PrimitivePicklerUnpickler[String]()
  implicit def booleanPicklerUnpickler(implicit format: PickleFormat): PrimitivePicklerUnpickler[Boolean] = new PrimitivePicklerUnpickler[Boolean]()
  implicit def nullPicklerUnpickler(implicit format: PickleFormat): Pickler[Null] with Unpickler[Null] = new PrimitivePicklerUnpickler[Null]()
  // TODO: if you uncomment this one, it will shadow picklers/unpicklers for Int and String. why?!
  // TODO: due to the inability to implement module pickling/unpickling in a separate macro, I moved the logic into genPickler/genUnpickler
  // implicit def modulePicklerUnpickler[T <: Singleton](implicit format: PickleFormat): Pickler[T] with Unpickler[T] = macro ModulePicklerUnpicklerMacro.impl[T]
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
            def pickle(pickleeRaw: Any, builder: PickleBuilder): Unit = {
              builder.beginEntry(typeTag[$tpe], pickleeRaw)
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
