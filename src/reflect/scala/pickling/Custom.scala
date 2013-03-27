package scala.pickling

import scala.reflect.runtime.universe._
import language.experimental.macros
import scala.collection.immutable.::

trait CorePicklersUnpicklers extends GenPicklers with GenUnpicklers {
  class PrimitivePicklerUnpickler[T: TypeTag](implicit val format: PickleFormat) extends Pickler[T] with Unpickler[T] {
    def pickle(picklee: T, builder: PickleBuilder): Unit = {
      builder.beginEntry(picklee)
      // builder.endEntry()
    }
    def unpickle(tag: TypeTag[_], reader: PickleReader): Any = {
      reader.beginEntry()
      val result = reader.readPrimitive()
      // reader.endEntry()
      result
    }
  }

  implicit def intPicklerUnpickler(implicit format: PickleFormat): Pickler[Int] with Unpickler[Int] = new PrimitivePicklerUnpickler[Int]
  implicit def stringPicklerUnpickler(implicit format: PickleFormat): Pickler[String] with Unpickler[String] = new PrimitivePicklerUnpickler[String]
  implicit def booleanPicklerUnpickler(implicit format: PickleFormat): Pickler[Boolean] with Unpickler[Boolean] = new PrimitivePicklerUnpickler[Boolean]
  implicit def nullPicklerUnpickler(implicit format: PickleFormat): Pickler[Null] with Unpickler[Null] = new PrimitivePicklerUnpickler[Null]
  implicit def genArrayPickler[T](implicit format: PickleFormat): Pickler[Array[T]] with Unpickler[Array[T]] = macro ArrayPicklerUnpicklerMacro.impl[T]
  implicit def genListPickler[T](implicit format: PickleFormat): Pickler[::[T]] with Unpickler[::[T]] = macro ListPicklerUnpicklerMacro.impl[T]
  // TODO: if you uncomment this one, it will shadow picklers/unpicklers for Int and String. why?!
  // TODO: due to the inability to implement module pickling/unpickling in a separate macro, I moved the logic into genPickler/genUnpickler
  // implicit def modulePicklerUnpickler[T <: Singleton](implicit format: PickleFormat): Pickler[T] with Unpickler[T] = macro ModulePicklerUnpicklerMacro.impl[T]
}

trait ArrayPicklerUnpicklerMacro extends Macro {
  def impl[T: c.WeakTypeTag](format: c.Tree): c.Tree = ???
}

trait ListPicklerUnpicklerMacro extends Macro {
  // TODO: can't make this work, because then genArrayPickler and getListPickler clash
  // def impl[T, Coll[_] <: List[_]](format: c.Tree): c.Tree = {
  def impl[T: c.WeakTypeTag](format: c.Tree): c.Tree = {
    import c.universe._
    // val Apply(TypeApply(_, teltpe :: thktpe :: Nil), _) = c.macroApplication
    // val eltpe = teltpe.tpe
    // val hktpe = thktpe.tpe
    // val tpe = appliedType(hktpe, List(eltpe))
    val tpe = weakTypeOf[::[T]]
    val eltpe = weakTypeOf[T]
    val elpickler = c.typeCheck(q"implicitly[Pickler[$eltpe]]")
    val elunpickler = c.typeCheck(q"implicitly[Unpickler[$eltpe]]")
    val eltag = c.typeCheck(q"implicitly[scala.reflect.runtime.universe.TypeTag[$eltpe]]")
    val isPrimitive = eltpe.typeSymbol.asClass.isPrimitive
    val picklerUnpickler = {
      c.topLevelRef(syntheticPicklerUnpicklerQualifiedName(tpe)) orElse c.introduceTopLevel(syntheticPackageName, {
        q"""
          class ${syntheticPicklerUnpicklerName(tpe)} extends scala.pickling.Pickler[$tpe] with scala.pickling.Unpickler[$tpe] {
            import scala.reflect.runtime.universe._
            import scala.pickling._
            import scala.pickling.`package`.PickleOps
            import scala.collection.mutable.ListBuffer
            implicit val format = new ${format.tpe}()
            implicit val elpickler: Pickler[$eltpe] = $elpickler
            implicit val elunpickler: Unpickler[$eltpe] = $elunpickler
            implicit val eltag: scala.reflect.runtime.universe.TypeTag[$eltpe] = $eltag
            def pickle(picklee: $tpe, builder: PickleBuilder): Unit = {
              if (!$isPrimitive) throw new PicklingException(s"implementation restriction: non-primitive lists aren't supported")
              builder.beginEntry()
              // TODO: this needs to be adjusted to work with non-primitive types
              // 1) elisions might need to be set on per-element basis
              // 2) val elpicker needs to be turned into def elpickler(el: $eltpe) which would do dispatch
              // 3) hint pinning would need to work with potentially nested picklings of elements
              // ============
              builder.hintKnownSize(picklee.length * 4 + 1000)
              builder.hintStaticallyElidedType()
              builder.hintTag(eltag)
              builder.pinHints()
              // ============
              val length = picklee.length
              val arr = picklee.toArray
              builder.beginCollection(length)
              var i = 0
              while (i < length) {
                // TODO: change putElement to get rid of the lambda
                // builder.putElement(b => elpickler.pickle(arr(i), b))
                elpickler.pickle(arr(i), builder)
                i += 1
              }
              builder.unpinHints()
              builder.endCollection()
              builder.endEntry()
            }
            def unpickle(tag: TypeTag[_], reader: PickleReader): Any = {
              if (!$isPrimitive) throw new PicklingException(s"implementation restriction: non-primitive lists aren't supported")
              var builder = ListBuffer[$eltpe]()
              val arrReader = reader.beginCollection()
              // TODO: this needs to be adjusted to work with non-primitive types
              arrReader.hintStaticallyElidedType()
              arrReader.hintTag(eltag)
              arrReader.pinHints()
              val length = arrReader.readLength()
              var i = 0
              while (i < length) {
                builder += elunpickler.unpickle(eltag, arrReader).asInstanceOf[$eltpe]
                i += 1
              }
              arrReader.unpinHints()
              arrReader.endCollection()
              builder.result
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
