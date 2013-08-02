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
    val isPrimitive = elemTag.tpe.isEffectivelyPrimitive

    def pickle(coll: Coll[T], builder: PBuilder): Unit = {
      builder.hintTag(collTag)
      builder.beginEntry(coll)

      if (coll.isInstanceOf[IndexedSeq[_]]) builder.beginCollection(coll.size)
      else builder.beginCollection(0)

      if (isPrimitive) {
        builder.hintStaticallyElidedType()
        builder.hintTag(elemTag)
        builder.pinHints()
      }

      var i = 0
      coll.asInstanceOf[Traversable[T]].foreach { (elem: T) =>
        builder putElement { b =>
          if (!isPrimitive) b.hintTag(elemTag)
          elemPickler.pickle(elem, b)
        }
        i += 1
      }

      if (isPrimitive) builder.unpinHints()
      builder.endCollection(i)
      builder.endEntry()
    }

    def unpickle(tpe: => FastTypeTag[_], preader: PReader): Any = {
      val reader = preader.beginCollection()

      if (isPrimitive) {
        reader.hintStaticallyElidedType()
        reader.hintTag(elemTag)
        reader.pinHints()
      }

      val length = reader.readLength()
      val builder = cbf.apply() // builder with element type T
      var i = 0
      while (i < length) {
        val r = reader.readElement()
        r.beginEntryNoTag()
        val elem = elemUnpickler.unpickle(elemTag, r)
        r.endEntry()
        builder += elem.asInstanceOf[T]
        i = i + 1
      }

      if (isPrimitive) reader.unpinHints()
      preader.endCollection()
      builder.result
    }
  }
}

trait CorePicklersUnpicklers extends GenPicklers with GenUnpicklers with LowPriorityPicklersUnpicklers {
  class PrimitivePicklerUnpickler[T] extends SPickler[T] with Unpickler[T] {
    val format = null // not used
    def pickle(picklee: T, builder: PBuilder): Unit = {
      builder.beginEntry(picklee)
      builder.endEntry()
    }
    def unpickle(tag: => FastTypeTag[_], reader: PReader): Any = {
      reader.readPrimitive()
    }
  }

  // TODO: figure out why removing these pickler/unpicklers slows down evactor1
  implicit val bytePicklerUnpickler: SPickler[Byte] with Unpickler[Byte] = new PrimitivePicklerUnpickler[Byte]
  implicit val shortPicklerUnpickler: SPickler[Short] with Unpickler[Short] = new PrimitivePicklerUnpickler[Short]
  implicit val charPicklerUnpickler: SPickler[Char] with Unpickler[Char] = new PrimitivePicklerUnpickler[Char]
  implicit val intPicklerUnpickler: SPickler[Int] with Unpickler[Int] = new PrimitivePicklerUnpickler[Int]
  implicit val longPicklerUnpickler: SPickler[Long] with Unpickler[Long] = new PrimitivePicklerUnpickler[Long]
  implicit val stringPicklerUnpickler: SPickler[String] with Unpickler[String] = new PrimitivePicklerUnpickler[String]
  implicit val booleanPicklerUnpickler: SPickler[Boolean] with Unpickler[Boolean] = new PrimitivePicklerUnpickler[Boolean]
  implicit val floatPicklerUnpickler: SPickler[Float] with Unpickler[Float] = new PrimitivePicklerUnpickler[Float]
  implicit val doublePicklerUnpickler: SPickler[Double] with Unpickler[Double] = new PrimitivePicklerUnpickler[Double]
  implicit val nullPicklerUnpickler: SPickler[Null] with Unpickler[Null] = new PrimitivePicklerUnpickler[Null]

  implicit def genListPickler[T](implicit format: PickleFormat): SPickler[::[T]] with Unpickler[::[T]] =
    macro Compat.ListPicklerUnpicklerMacro_impl[T]
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
          builder.beginEntry()
          if ($isPrimitive) {
            builder.hintStaticallyElidedType()
            builder.hintTag(eltag)
            builder.pinHints()
          }
          val arr = ${mkArray(q"picklee")}
          val length = arr.length
          builder.beginCollection(arr.length)
          var i = 0
          while (i < arr.length) {
            builder putElement { b =>
              if (!$isPrimitive) b.hintTag(eltag)
              elpickler.pickle(arr(i), b)
            }
            i += 1
          }
          if ($isPrimitive) builder.unpinHints()
          builder.endCollection(i)
          builder.endEntry()
        }
        def unpickle(tag: => scala.pickling.FastTypeTag[_], reader: PReader): Any = {
          var buffer = ${mkBuffer(eltpe)}
          val arrReader = reader.beginCollection()
          if ($isPrimitive) {
            arrReader.hintStaticallyElidedType()
            arrReader.hintTag(eltag)
            arrReader.pinHints()
          }
          val length = arrReader.readLength()
          var i = 0
          while (i < length) {
            val r = arrReader.readElement()
            r.beginEntryNoTag()
            val elem = elunpickler.unpickle(eltag, r)
            r.endEntry()
            buffer += elem.asInstanceOf[$eltpe]
            i += 1
          }
          if ($isPrimitive) arrReader.unpinHints()
          arrReader.endCollection()
          ${mkResult(q"buffer")}
        }
      }
      $picklerUnpicklerName
    """
  }
}
