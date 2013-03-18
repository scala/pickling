package scala.pickling

import scala.reflect.runtime.universe._
import language.experimental.macros

class Custom

trait CorePicklersUnpicklers extends GenPicklers with GenUnpicklers {
  // TODO: since we don't know precise types of builder and reader, we can't do optimizations here!!
  // I think we can fix this problem with type macros, so let's not worry much for now - I'll handle it when looking into custom picklers
  class PrimitivePicklerUnpickler[T: TypeTag](implicit val format: PickleFormat) extends Pickler[T] with Unpickler[T] {
    type PickleFormatType = PickleFormat
    type PickleBuilderType = PickleBuilder
    def pickle(picklee: Any, builder: PickleBuilderType): Unit = {
      builder.beginEntry(typeOf[T], picklee)
      builder.endEntry()
    }
    type PickleReaderType = PickleReader
    def unpickle(tpe: Type, reader: PickleReaderType): Any = {
      // NOTE: here we essentially require that ints and strings are primitives for all readers
      // TODO: discuss that and see whether it defeats all the purpose of abstracting primitives away from picklers
      // TODO: validate that tpe and typeOf[T] work together
      reader.readPrimitive(typeOf[T])
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
      val builderTpe = pickleBuilderType(format)
      val readerTpe = pickleReaderType(format)
      c.topLevelRef(syntheticPicklerUnpicklerQualifiedName(tpe, builderTpe, readerTpe)) orElse c.introduceTopLevel(syntheticPackageName, {
        q"""
          class ${syntheticPicklerUnpicklerName(tpe, builderTpe, readerTpe)} extends scala.pickling.Pickler[$tpe] with scala.pickling.Unpickler[$tpe] {
            import scala.reflect.runtime.universe._
            import scala.pickling._
            import scala.pickling.`package`.PickleOps
            type PickleFormatType = ${format.tpe}
            implicit val format = new PickleFormatType()
            type PickleBuilderType = ${pickleBuilderType(format)}
            def pickle(pickleeRaw: Any, builder: PickleBuilderType): Unit = {
              builder.beginEntry(typeOf[$tpe], pickleeRaw)
              builder.endEntry()
            }
            type PickleReaderType = ${pickleReaderType(format)}
            def unpickle(tpe: Type, reader: PickleReaderType): Any = {
              $module
            }
          }
        """
      })
    }
    q"new $picklerUnpickler"
  }
}
