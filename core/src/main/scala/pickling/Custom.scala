package scala.pickling

import scala.language.experimental.macros
import scala.language.higherKinds

import scala.reflect.runtime.universe._

import scala.collection.immutable.::
import scala.collection.generic.CanBuildFrom
import scala.collection.IndexedSeq

trait LowPriorityPicklersUnpicklers {

  implicit def traversablePickler[T: FastTypeTag, Coll[_] <: Traversable[_]]
    (implicit elemPickler: SPickler[T], elemUnpickler: Unpickler[T],
              pf: PickleFormat, cbf: CanBuildFrom[Coll[_], T, Coll[T]],
              collTag: FastTypeTag[Coll[T]]): SPickler[Coll[T]] with Unpickler[Coll[T]] =
    new SPickler[Coll[T]] with Unpickler[Coll[T]] {

    val format: PickleFormat = pf
    val elemTag  = implicitly[FastTypeTag[T]]

    def pickle(coll: Coll[T], builder: PBuilder): Unit = {
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

    def unpickle(tpe: => FastTypeTag[_], preader: PReader): Any = {
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
  class PrimitivePicklerUnpickler[T](implicit val format: PickleFormat) extends SPickler[T] with Unpickler[T] {
    def pickle(picklee: T, builder: PBuilder): Unit = {
      builder.beginEntry(picklee)
      builder.endEntry()
    }
    def unpickle(tag: => FastTypeTag[_], reader: PReader): Any = {
      reader.readPrimitive()
    }
  }

  // TODO: figure out why removing these pickler/unpicklers slows down evactor1
  implicit def bytePicklerUnpickler(implicit format: PickleFormat): SPickler[Byte] with Unpickler[Byte] = new PrimitivePicklerUnpickler[Byte]
  implicit def shortPicklerUnpickler(implicit format: PickleFormat): SPickler[Short] with Unpickler[Short] = new PrimitivePicklerUnpickler[Short]
  implicit def charPicklerUnpickler(implicit format: PickleFormat): SPickler[Char] with Unpickler[Char] = new PrimitivePicklerUnpickler[Char]
  implicit def intPicklerUnpickler(implicit format: PickleFormat): SPickler[Int] with Unpickler[Int] = new PrimitivePicklerUnpickler[Int]
  implicit def longPicklerUnpickler(implicit format: PickleFormat): SPickler[Long] with Unpickler[Long] = new PrimitivePicklerUnpickler[Long]
  implicit def stringPicklerUnpickler(implicit format: PickleFormat): SPickler[String] with Unpickler[String] = new PrimitivePicklerUnpickler[String]
  implicit def booleanPicklerUnpickler(implicit format: PickleFormat): SPickler[Boolean] with Unpickler[Boolean] = new PrimitivePicklerUnpickler[Boolean]
  implicit def floatPicklerUnpickler(implicit format: PickleFormat): SPickler[Float] with Unpickler[Float] = new PrimitivePicklerUnpickler[Float]
  implicit def doublePicklerUnpickler(implicit format: PickleFormat): SPickler[Double] with Unpickler[Double] = new PrimitivePicklerUnpickler[Double]
  implicit def nullPicklerUnpickler(implicit format: PickleFormat): SPickler[Null] with Unpickler[Null] = new PrimitivePicklerUnpickler[Null]

  implicit def genListPickler[T](implicit format: PickleFormat): SPickler[::[T]] with Unpickler[::[T]] = macro ListPicklerUnpicklerMacro.impl[T]
  // TODO: figure out why this is slower than traversablePickler
  // implicit def genVectorPickler[T](implicit format: PickleFormat): Pickler[Vector[T]] with Unpickler[Vector[T]] = macro VectorPicklerUnpicklerMacro.impl[T]
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
    val isPrimitive = eltpe.isEffectivelyPrimitive
    val picklerUnpicklerName = c.fresh(syntheticPicklerUnpicklerName(tpe).toTermName)
    q"""
      implicit object $picklerUnpicklerName extends scala.pickling.SPickler[$tpe] with scala.pickling.Unpickler[$tpe] {
        import scala.reflect.runtime.universe._
        import scala.pickling._
        import scala.pickling.`package`.PickleOps
        val format = new ${format.tpe}()
        implicit val elpickler: SPickler[$eltpe] = {
          val elpickler = "bam!"
          implicitly[SPickler[$eltpe]]
        }
        implicit val elunpickler: Unpickler[$eltpe] = {
          val elunpickler = "bam!"
          implicitly[Unpickler[$eltpe]]
        }
        implicit val eltag: scala.pickling.FastTypeTag[$eltpe] = {
          val eltag = "bam!"
          implicitly[scala.pickling.FastTypeTag[$eltpe]]
        }
        def pickle(picklee: $tpe, builder: PBuilder): Unit = {
          if (!$isPrimitive) throw new PicklingException(s"implementation restriction: non-primitive collections aren't supported")
          builder.beginEntry()
          // TODO: this needs to be adjusted to work with non-primitive types
          // 1) elisions might need to be set on per-element basis
          // 2) val elpicker needs to be turned into def elpickler(el: $$eltpe) which would do dispatch
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
        def unpickle(tag: => scala.pickling.FastTypeTag[_], reader: PReader): Any = {
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
            buffer += arrReader.readPrimitive().asInstanceOf[$eltpe]
            arrReader.endEntry()
            i += 1
          }
          arrReader.unpinHints()
          arrReader.endCollection()
          ${mkResult(q"buffer")}
        }
      }
      $picklerUnpicklerName
    """
  }
}
