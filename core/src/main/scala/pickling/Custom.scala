package scala.pickling

import scala.reflect.runtime.universe._
import language.experimental.macros
import scala.collection.immutable.::
import scala.collection.generic.CanBuildFrom
import scala.collection.IndexedSeq

trait LowPriorityPicklersUnpicklers {

  implicit def traversablePickler[T: TypeTag, Coll[_] <: Traversable[_]]
    (implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
              pf: PickleFormat, cbf: CanBuildFrom[Coll[_], T, Coll[T]],
              collTag: TypeTag[Coll[T]]): Pickler[Coll[T]] with Unpickler[Coll[T]] =
    new Pickler[Coll[T]] with Unpickler[Coll[T]] {

    val format: PickleFormat = pf
    val elemTag  = typeTag[T]

    def pickle(coll: Coll[T], builder: PickleBuilder): Unit = {
      builder.hintTag(collTag)
      builder.beginEntry(coll)

      if (coll.isInstanceOf[IndexedSeq[_]]) builder.beginCollection(coll.size)
      else builder.beginCollection(0)

      builder.hintStaticallyElidedType()
      builder.hintTag(elemTag)
      builder.pinHints()

      var i = 0
      coll.asInstanceOf[Traversable[T]].foreach { (elem: T) =>
        builder.beginEntry(elem)
        builder.endEntry()
        i += 1
      }

      builder.unpinHints()
      builder.endCollection(i)
      builder.endEntry()
    }

    def unpickle(tpe: => TypeTag[_], preader: PickleReader): Any = {
      val reader = preader.beginCollection()
      reader.hintStaticallyElidedType()
      reader.hintTag(elemTag)
      reader.pinHints()

      val length = reader.readLength()
      var builder = cbf.apply() // builder with element type T
      var i = 0
      while (i < length) {
        reader.beginEntry()
        builder += reader.readPrimitive().asInstanceOf[T]
        reader.endEntry()
        i = i + 1
      }

      builder.result
    }
  }
}

trait CorePicklersUnpicklers extends GenPicklers with GenUnpicklers with LowPriorityPicklersUnpicklers {
  class PrimitivePicklerUnpickler[T: TypeTag](implicit val format: PickleFormat) extends Pickler[T] with Unpickler[T] {
    def pickle(picklee: T, builder: PickleBuilder): Unit = {
      builder.beginEntry(picklee)
      builder.endEntry()
    }
    def unpickle(tag: => TypeTag[_], reader: PickleReader): Any = {
      reader.readPrimitive()
    }
  }

  implicit def intPicklerUnpickler(implicit format: PickleFormat): Pickler[Int] with Unpickler[Int] = new PrimitivePicklerUnpickler[Int]
  implicit def stringPicklerUnpickler(implicit format: PickleFormat): Pickler[String] with Unpickler[String] = new PrimitivePicklerUnpickler[String]
  implicit def booleanPicklerUnpickler(implicit format: PickleFormat): Pickler[Boolean] with Unpickler[Boolean] = new PrimitivePicklerUnpickler[Boolean]
  implicit def nullPicklerUnpickler(implicit format: PickleFormat): Pickler[Null] with Unpickler[Null] = new PrimitivePicklerUnpickler[Null]

  implicit def genArrayPickler[T: TypeTag](implicit format: PickleFormat): Pickler[Array[T]] with Unpickler[Array[T]] = //macro ArrayPicklerUnpicklerMacro.impl[T]
    new ArrayPickler[T]

  implicit def genListPickler[T](implicit format: PickleFormat): Pickler[::[T]] with Unpickler[::[T]] = macro Compat.ListPicklerUnpicklerMacro_impl[T]

  implicit def tuple2Pickler[S: TypeTag, T: TypeTag]
    (implicit comp1Pickler: Pickler[S], comp1Unpickler: Unpickler[S],
              comp2Pickler: Pickler[T], comp2Unpickler: Unpickler[T],
              format: PickleFormat,
              tupleTag: TypeTag[(S, T)]): Pickler[(S, T)] with Unpickler[(S, T)] =
    new Tuple2Pickler[S, T]

  implicit def tuple3Pickler[T1: TypeTag, T2: TypeTag, T3: TypeTag]
    (implicit pickler1: Pickler[T1], unpickler1: Unpickler[T1],
              pickler2: Pickler[T2], unpickler2: Unpickler[T2],
              pickler3: Pickler[T3], unpickler3: Unpickler[T3],
              format: PickleFormat,
              tupleTag: TypeTag[(T1, T2, T3)]): Pickler[(T1, T2, T3)] with Unpickler[(T1, T2, T3)] =
    new Tuple3Pickler[T1, T2, T3]

  // TODO: if you uncomment this one, it will shadow picklers/unpicklers for Int and String. why?!
  // TODO: due to the inability to implement module pickling/unpickling in a separate macro, I moved the logic into genPickler/genUnpickler
  // implicit def modulePicklerUnpickler[T <: Singleton](implicit format: PickleFormat): Pickler[T] with Unpickler[T] = macro ModulePicklerUnpicklerMacro.impl[T]
}

trait ArrayPicklerUnpicklerMacro extends CollectionPicklerUnpicklerMacro {
  import c.universe._
  import definitions._
  def mkType(eltpe: c.Type) = appliedType(ArrayClass.toTypeConstructor, List(eltpe))
  def mkArray(picklee: c.Tree) = q"$picklee"
  def mkBuffer(eltpe: c.Type) = q"scala.collection.mutable.ArrayBuffer[$eltpe]()"
  def mkResult(buffer: c.Tree) = q"$buffer.toArray"
}

trait ListPicklerUnpicklerMacro extends CollectionPicklerUnpicklerMacro {
  import c.universe._
  import definitions._
  lazy val ConsClass = c.mirror.staticClass("scala.collection.immutable.$colon$colon")
  def mkType(eltpe: c.Type) = appliedType(ConsClass.toTypeConstructor, List(eltpe))
  def mkArray(picklee: c.Tree) = q"$picklee.toArray"
  def mkBuffer(eltpe: c.Type) = q"scala.collection.mutable.ListBuffer[$eltpe]()"
  def mkResult(buffer: c.Tree) = q"$buffer.toList"
}

trait VectorPicklerUnpicklerMacro extends CollectionPicklerUnpicklerMacro {
  import c.universe._
  import definitions._
  lazy val VectorClass = c.mirror.staticClass("scala.collection.immutable.Vector")
  def mkType(eltpe: c.Type) = appliedType(VectorClass.toTypeConstructor, List(eltpe))
  def mkArray(picklee: c.Tree) = q"$picklee.toArray"
  def mkBuffer(eltpe: c.Type) = q"scala.collection.mutable.ListBuffer[$eltpe]()"
  def mkResult(buffer: c.Tree) = q"$buffer.toVector"
}

trait CollectionPicklerUnpicklerMacro extends Macro {
  def mkType(eltpe: c.Type): c.Type
  def mkArray(picklee: c.Tree): c.Tree
  def mkBuffer(eltpe: c.Type): c.Tree
  def mkResult(buffer: c.Tree): c.Tree

  def impl[T: c.WeakTypeTag](format: c.Tree): c.Tree = {
    import c.universe._
    val tpe = mkType(weakTypeOf[T])
    val eltpe = weakTypeOf[T]
    val isPrimitive = eltpe.typeSymbol.asClass.isPrimitive
    val picklerUnpickler = {
      c.topLevelRef(syntheticPicklerUnpicklerQualifiedName(tpe)) orElse c.introduceTopLevel(syntheticPackageName, {
        q"""
          class ${syntheticPicklerUnpicklerName(tpe)} extends scala.pickling.Pickler[$tpe] with scala.pickling.Unpickler[$tpe] {
            import scala.reflect.runtime.universe._
            import scala.pickling._
            import scala.pickling.`package`.PickleOps
            implicit val format = new ${format.tpe}()
            implicit val elpickler: Pickler[$eltpe] = {
              val elpickler = "bam!"
              implicitly[Pickler[$eltpe]]
            }
            implicit val elunpickler: Unpickler[$eltpe] = {
              val elunpickler = "bam!"
              implicitly[Unpickler[$eltpe]]
            }
            implicit val eltag: scala.reflect.runtime.universe.TypeTag[$eltpe] = {
              val eltag = "bam!"
              implicitly[scala.reflect.runtime.universe.TypeTag[$eltpe]]
            }
            def pickle(picklee: $tpe, builder: PickleBuilder): Unit = {
              if (!$isPrimitive) throw new PicklingException(s"implementation restriction: non-primitive collections aren't supported")
              builder.beginEntry()
              // TODO: this needs to be adjusted to work with non-primitive types
              // 1) elisions might need to be set on per-element basis
              // 2) val elpicker needs to be turned into def elpickler(el: $eltpe) which would do dispatch
              // 3) hint pinning would need to work with potentially nested picklings of elements
              // ============
              builder.hintStaticallyElidedType()
              builder.hintTag(eltag)
              builder.pinHints()
              // ============
              val arr = ${mkArray(q"picklee")}
              val length = arr.length
              builder.beginCollection(arr.length)
              var i = 0
              while (i < arr.length) {
                builder.putElement(b => elpickler.pickle(arr(i), b))
                i += 1
              }
              builder.unpinHints()
              builder.endCollection(i)
              builder.endEntry()
            }
            def unpickle(tag: => TypeTag[_], reader: PickleReader): Any = {
              if (!$isPrimitive) throw new PicklingException(s"implementation restriction: non-primitive collections aren't supported")
              var buffer = ${mkBuffer(eltpe)}
              val arrReader = reader.beginCollection()
              // TODO: this needs to be adjusted to work with non-primitive types
              arrReader.hintStaticallyElidedType()
              arrReader.hintTag(eltag)
              arrReader.pinHints()
              val length = arrReader.readLength()
              var i = 0
              while (i < length) {
                arrReader.beginEntry()
                buffer += elunpickler.unpickle(eltag, arrReader.readElement()).asInstanceOf[$eltpe]
                arrReader.endEntry()
                i += 1
              }
              arrReader.unpinHints()
              arrReader.endCollection()
              ${mkResult(q"buffer")}
            }
          }
        """
      })
    }
    q"new $picklerUnpickler"
  }
}
