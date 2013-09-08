package scala.pickling

import scala.language.experimental.macros
import scala.language.higherKinds

import scala.reflect.runtime.universe._

import scala.collection.immutable.::
import scala.collection.generic.CanBuildFrom
import scala.collection.IndexedSeq
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.collection.immutable

class PicklerUnpicklerNotFound[T] extends SPickler[T] with Unpickler[T] {
  val format = null // not used
  def pickle(picklee: T, builder: PBuilder): Unit = ???
  def unpickle(tag: => FastTypeTag[_], reader: PReader): Any = ???
}

trait CorePicklersUnpicklers extends GenPicklers with GenUnpicklers {
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
  implicit val booleanPicklerUnpickler: SPickler[Boolean] with Unpickler[Boolean] = new PrimitivePicklerUnpickler[Boolean]
  implicit val floatPicklerUnpickler: SPickler[Float] with Unpickler[Float] = new PrimitivePicklerUnpickler[Float]
  implicit val doublePicklerUnpickler: SPickler[Double] with Unpickler[Double] = new PrimitivePicklerUnpickler[Double]
  implicit val nullPicklerUnpickler: SPickler[Null] with Unpickler[Null] = new PrimitivePicklerUnpickler[Null]
  implicit val stringPicklerUnpickler: SPickler[String] with Unpickler[String] =  new PrimitivePicklerUnpickler[String]

  implicit def refPickler: SPickler[refs.Ref] = throw new Error("cannot pickle refs") // TODO: make this a macro
  implicit val refUnpickler: Unpickler[refs.Ref] = new PrimitivePicklerUnpickler[refs.Ref]

  implicit def genArrayPickler[T >: Null](implicit format: PickleFormat): SPickler[Array[T]] with Unpickler[Array[T]] = macro Compat.ArrayPicklerUnpicklerMacro_impl[T]
  implicit def genSeqPickler[T](implicit format: PickleFormat): SPickler[Seq[T]] with Unpickler[Seq[T]] = macro Compat.SeqPicklerUnpicklerMacro_impl[T]
  implicit def genIterablePickler[T](implicit format: PickleFormat): SPickler[Iterable[T]] with Unpickler[Iterable[T]] = macro Compat.IterablePicklerUnpicklerMacro_impl[T]
  implicit def genListPickler[T](implicit format: PickleFormat): SPickler[::[T]] with Unpickler[::[T]] = macro Compat.ListPicklerUnpicklerMacro_impl[T]
  implicit def genVectorPickler[T](implicit format: PickleFormat): SPickler[Vector[T]] with Unpickler[Vector[T]] = macro Compat.VectorPicklerUnpicklerMacro_impl[T]
  implicit def genImmSetPickler[T](implicit format: PickleFormat): SPickler[Set[T]] with Unpickler[Set[T]] = macro Compat.ImmSetPicklerUnpicklerMacro_impl[T]
  implicit def genImmSortedSetPickler[T](implicit format: PickleFormat): SPickler[immutable.SortedSet[T]] with Unpickler[immutable.SortedSet[T]] = macro Compat.ImmSortedSetPicklerUnpicklerMacro_impl[T]
  implicit def genMutSetPickler[T](implicit format: PickleFormat): SPickler[mutable.Set[T]] with Unpickler[mutable.Set[T]] = macro Compat.MutSetPicklerUnpicklerMacro_impl[T]
  implicit def genMutSortedSetPickler[T](implicit format: PickleFormat): SPickler[mutable.SortedSet[T]] with Unpickler[mutable.SortedSet[T]] = macro Compat.MutSortedSetPicklerUnpicklerMacro_impl[T]

  implicit def genImmMapPickler[K, V](implicit format: PickleFormat): SPickler[Map[K, V]] with Unpickler[Map[K, V]] = macro Compat.ImmMapPicklerUnpicklerMacro_impl[K, V]
  implicit def genImmSortedMapPickler[K, V](implicit format: PickleFormat): SPickler[immutable.SortedMap[K, V]] with Unpickler[immutable.SortedMap[K, V]] = macro Compat.ImmSortedMapPicklerUnpicklerMacro_impl[K, V]
  implicit def genMutMapPickler[K, V](implicit format: PickleFormat): SPickler[mutable.Map[K, V]] with Unpickler[mutable.Map[K, V]] = macro Compat.MutMapPicklerUnpicklerMacro_impl[K, V]

  // TODO: figure out why the macro-based version for ArrayBuffers is slower
  // implicit def genArrayBufferPickler[T](implicit format: PickleFormat): SPickler[ArrayBuffer[T]] with Unpickler[ArrayBuffer[T]] = macro Compat.ArrayBufferPicklerUnpicklerMacro_impl[T]
  implicit def genArrayBufferPickler[T: FastTypeTag](implicit
      elemPickler: SPickler[T], elemUnpickler: Unpickler[T],
      pf: PickleFormat, cbf: CanBuildFrom[ArrayBuffer[_], T, ArrayBuffer[T]],
      collTag: FastTypeTag[ArrayBuffer[T]]): SPickler[ArrayBuffer[T]] with Unpickler[ArrayBuffer[T]] = {
    new SPickler[ArrayBuffer[T]] with Unpickler[ArrayBuffer[T]] {
      val format: PickleFormat = pf
      val elemTag = implicitly[FastTypeTag[T]]
      val isEffectivelyFinal = elemTag.tpe.isEffectivelyFinal

      def pickle(coll: ArrayBuffer[T], builder: PBuilder): Unit = {
        builder.hintTag(collTag)
        builder.beginEntry(coll)

        if (coll.isInstanceOf[IndexedSeq[_]]) builder.beginCollection(coll.size)
        else builder.beginCollection(0)

        var i = 0
        coll.asInstanceOf[Traversable[T]].foreach { (elem: T) =>
          builder putElement (b => {
            if (isEffectivelyFinal) builder.hintStaticallyElidedType()
            builder.hintTag(elemTag)
            elemPickler.pickle(elem, b)
          })
          i += 1
        }

        builder.endCollection(i)
        builder.endEntry()
      }

      def unpickle(tpe: => FastTypeTag[_], preader: PReader): Any = {
        val reader = preader.beginCollection()

        val length = reader.readLength()
        val builder = cbf.apply() // builder with element type T
        var i = 0
        while (i < length) {
          if (isEffectivelyFinal) reader.hintStaticallyElidedType()
          reader.hintTag(elemTag)
          val r = reader.readElement()
          r.beginEntryNoTag()
          val elem = elemUnpickler.unpickle(elemTag, r)
          r.endEntry()
          builder += elem.asInstanceOf[T]
          i = i + 1
        }

        preader.endCollection()
        builder.result
      }
    }
  }
}

trait ArrayPicklerUnpicklerMacro extends CollectionPicklerUnpicklerMacro {
  import c.universe._
  import definitions._
  lazy val ConsClass = c.mirror.staticClass("scala.Array")
  def mkType(eltpe: c.Type) = appliedType(ConsClass.toTypeConstructor, List(eltpe))
  def mkArray(picklee: c.Tree) = q"$picklee.toArray"
  def mkBuffer(eltpe: c.Type) = q"scala.collection.mutable.ArrayBuffer[$eltpe]()"
  def mkResult(buffer: c.Tree) = q"$buffer.toArray"
}

trait SeqPicklerUnpicklerMacro extends CollectionPicklerUnpicklerMacro {
  import c.universe._
  import definitions._
  lazy val ConsClass = c.mirror.staticClass("scala.collection.Seq")
  def mkType(eltpe: c.Type) = appliedType(ConsClass.toTypeConstructor, List(eltpe))
  def mkArray(picklee: c.Tree) = q"$picklee.toArray"
  def mkBuffer(eltpe: c.Type) = q"scala.collection.mutable.ListBuffer[$eltpe]()"
  def mkResult(buffer: c.Tree) = q"$buffer.toSeq"
}

trait IterablePicklerUnpicklerMacro extends CollectionPicklerUnpicklerMacro {
  import c.universe._
  import definitions._
  lazy val CollClass = c.mirror.staticClass("scala.Iterable")
  def mkType(eltpe: c.Type) = appliedType(CollClass.toTypeConstructor, List(eltpe))
  def mkArray(picklee: c.Tree) = q"$picklee.toArray"
  def mkBuffer(eltpe: c.Type) = q"scala.collection.mutable.ListBuffer[$eltpe]()"
  def mkResult(buffer: c.Tree) = q"$buffer.toSeq"
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
  def mkBuffer(eltpe: c.Type) = q"new scala.collection.immutable.VectorBuilder[$eltpe]()"
  def mkResult(buffer: c.Tree) = q"$buffer.result"
}

trait ImmSetPicklerUnpicklerMacro extends CollectionPicklerUnpicklerMacro {
  import c.universe._
  import definitions._
  lazy val CollClass = c.mirror.staticClass("scala.collection.immutable.Set")
  def mkType(eltpe: c.Type) = appliedType(CollClass.toTypeConstructor, List(eltpe))
  def mkArray(picklee: c.Tree) = q"$picklee.toArray"
  def mkBuffer(eltpe: c.Type) = q"new scala.collection.mutable.SetBuilder[$eltpe, scala.collection.immutable.Set[$eltpe]](scala.collection.immutable.Set.empty[$eltpe])"
  def mkResult(buffer: c.Tree) = q"$buffer.result"
}

trait ImmSortedSetPicklerUnpicklerMacro extends CollectionPicklerUnpicklerMacro {
  import c.universe._
  import definitions._
  lazy val CollClass = c.mirror.staticClass("scala.collection.immutable.SortedSet")
  def mkType(eltpe: c.Type) = appliedType(CollClass.toTypeConstructor, List(eltpe))
  def mkArray(picklee: c.Tree) = q"$picklee.toArray"
  def mkBuffer(eltpe: c.Type) = q"new scala.collection.mutable.SetBuilder[$eltpe, scala.collection.immutable.SortedSet[$eltpe]](scala.collection.immutable.SortedSet.empty[$eltpe])"
  def mkResult(buffer: c.Tree) = q"$buffer.result"
}

trait MutSetPicklerUnpicklerMacro extends CollectionPicklerUnpicklerMacro {
  import c.universe._
  import definitions._
  lazy val CollClass = c.mirror.staticClass("scala.collection.mutable.Set")
  def mkType(eltpe: c.Type) = appliedType(CollClass.toTypeConstructor, List(eltpe))
  def mkArray(picklee: c.Tree) = q"$picklee.toArray"
  def mkBuffer(eltpe: c.Type) = q"new scala.collection.mutable.SetBuilder[$eltpe, scala.collection.mutable.Set[$eltpe]](scala.collection.mutable.Set.empty[$eltpe])"
  def mkResult(buffer: c.Tree) = q"$buffer.result"
}

trait MutSortedSetPicklerUnpicklerMacro extends CollectionPicklerUnpicklerMacro {
  import c.universe._
  import definitions._
  lazy val CollClass = c.mirror.staticClass("scala.collection.mutable.SortedSet")
  def mkType(eltpe: c.Type) = appliedType(CollClass.toTypeConstructor, List(eltpe))
  def mkArray(picklee: c.Tree) = q"$picklee.toArray"
  def mkBuffer(eltpe: c.Type) = q"new scala.collection.mutable.SetBuilder[$eltpe, scala.collection.mutable.SortedSet[$eltpe]](scala.collection.mutable.SortedSet.empty[$eltpe])"
  def mkResult(buffer: c.Tree) = q"$buffer.result"
}

trait ArrayBufferPicklerUnpicklerMacro extends CollectionPicklerUnpicklerMacro {
  import c.universe._
  import definitions._
  lazy val ArrayBufferClass = c.mirror.staticClass("scala.collection.mutable.ArrayBuffer")
  def mkType(eltpe: c.Type) = appliedType(ArrayBufferClass.toTypeConstructor, List(eltpe))
  def mkArray(picklee: c.Tree) = q"$picklee.toArray"
  def mkBuffer(eltpe: c.Type) = q"new scala.collection.mutable.ArrayBuffer[$eltpe]()"
  def mkResult(buffer: c.Tree) = q"$buffer"
}

trait ImmMapPicklerUnpicklerMacro extends MapPicklerUnpicklerMacro {
  import c.universe._
  import definitions._
  lazy val CollClass = c.mirror.staticClass("scala.collection.immutable.Map")
  def mkType(keytpe: c.Type, valtpe: c.Type) = appliedType(CollClass.toTypeConstructor, List(keytpe, valtpe))
  def mkArray(picklee: c.Tree) = q"$picklee.toArray"
  def mkBuffer(keytpe: c.Type, valtpe: c.Type) = q"new scala.collection.mutable.ListBuffer[($keytpe, $valtpe)]()"
  def mkResult(buffer: c.Tree) = q"$buffer.toMap"
}

trait ImmSortedMapPicklerUnpicklerMacro extends MapPicklerUnpicklerMacro {
  import c.universe._
  import definitions._
  lazy val CollClass = c.mirror.staticClass("scala.collection.immutable.SortedMap")
  def mkType(keytpe: c.Type, valtpe: c.Type) = appliedType(CollClass.toTypeConstructor, List(keytpe, valtpe))
  def mkArray(picklee: c.Tree) = q"$picklee.toArray"
  def mkBuffer(keytpe: c.Type, valtpe: c.Type) = q"new scala.collection.mutable.MapBuilder[$keytpe, $valtpe, scala.collection.immutable.SortedMap[$keytpe, $valtpe]](scala.collection.immutable.SortedMap.empty[$keytpe, $valtpe])"
  def mkResult(buffer: c.Tree) = q"$buffer.result"
}

trait MutMapPicklerUnpicklerMacro extends MapPicklerUnpicklerMacro {
  import c.universe._
  import definitions._
  lazy val CollClass = c.mirror.staticClass("scala.collection.mutable.Map")
  def mkType(keytpe: c.Type, valtpe: c.Type) = appliedType(CollClass.toTypeConstructor, List(keytpe, valtpe))
  def mkArray(picklee: c.Tree) = q"$picklee.toArray"
  def mkBuffer(keytpe: c.Type, valtpe: c.Type) = q"new scala.collection.mutable.MapBuilder[$keytpe, $valtpe, scala.collection.mutable.Map[$keytpe, $valtpe]](scala.collection.mutable.Map.empty[$keytpe, $valtpe])"
  def mkResult(buffer: c.Tree) = q"$buffer.result"
}

trait MapPicklerUnpicklerMacro extends Macro {
  def mkType(keytpe: c.Type, valtpe: c.Type): c.Type
  def mkArray(picklee: c.Tree): c.Tree
  def mkBuffer(keytpe: c.Type, valtpe: c.Type): c.Tree
  def mkResult(buffer: c.Tree): c.Tree

  def impl[K: c.WeakTypeTag, V: c.WeakTypeTag](format: c.Tree): c.Tree = {
    import c.universe._
    import definitions._

    val keytpe = weakTypeOf[K]
    val valtpe = weakTypeOf[V]
    val tpe = mkType(keytpe, valtpe)
    val eltpe = weakTypeOf[(K, V)]
    val isPrimitive = eltpe.isEffectivelyPrimitive
    val isFinal = eltpe.isEffectivelyFinal

    val picklerUnpicklerName = c.fresh(syntheticPicklerUnpicklerName(tpe).toTermName)

    q"""
      implicit object $picklerUnpicklerName extends scala.pickling.SPickler[$tpe] with scala.pickling.Unpickler[$tpe] {
        import scala.reflect.runtime.universe._
        import scala.pickling._
        import scala.pickling.`package`.PickleOps

        val format = implicitly[${format.tpe}]

        val elpickler: SPickler[$eltpe] = {
          val elpickler = "bam!"
          implicitly[SPickler[$eltpe]]
        }
        val elunpickler: Unpickler[$eltpe] = {
          val elunpickler = "bam!"
          implicitly[Unpickler[$eltpe]]
        }
        val eltag: scala.pickling.FastTypeTag[$eltpe] = {
          val eltag = "bam!"
          implicitly[scala.pickling.FastTypeTag[$eltpe]]
        }
        val colltag: scala.pickling.FastTypeTag[$tpe] = {
          val colltag = "bam!"
          implicitly[scala.pickling.FastTypeTag[$tpe]]
        }

        def pickle(picklee: $tpe, builder: PBuilder): Unit = {
          builder.hintTag(colltag)
          ${
            if (eltpe =:= IntTpe) q"builder.hintKnownSize(picklee.length * 4 + 100)".asInstanceOf[Tree]
            else q"".asInstanceOf[Tree]
          }
          builder.beginEntry(picklee)
          ${
            if (isPrimitive) q"builder.hintStaticallyElidedType(); builder.hintTag(eltag); builder.pinHints()".asInstanceOf[Tree]
            else q"".asInstanceOf[Tree]
          }
          val arr = ${mkArray(q"picklee")}
          val length = arr.length
          builder.beginCollection(arr.length)
          var i = 0
          while (i < arr.length) {
            builder putElement { b =>
              ${
                if (!isPrimitive && !isFinal) q"""
                  b.hintTag(eltag)
                  arr(i).pickleInto(b)
                """.asInstanceOf[Tree] else if (!isPrimitive && isFinal) q"""
                  b.hintTag(eltag)
                  b.hintStaticallyElidedType()
                  arr(i).pickleInto(b)
                """.asInstanceOf[Tree] else q"""
                  elpickler.pickle(arr(i), b)
                """.asInstanceOf[Tree]
              }
            }
            i += 1
          }
          ${
            if (isPrimitive) q"builder.unpinHints()".asInstanceOf[Tree]
            else q"".asInstanceOf[Tree]
          }
          builder.endCollection(i)
          builder.endEntry()
        }
        def unpickle(tag: => scala.pickling.FastTypeTag[_], reader: PReader): Any = {
          val arrReader = reader.beginCollection()
          ${
            if (isPrimitive) q"arrReader.hintStaticallyElidedType(); arrReader.hintTag(eltag); arrReader.pinHints()".asInstanceOf[Tree]
            else q"".asInstanceOf[Tree]
          }
          val length = arrReader.readLength()
          var buffer = ${mkBuffer(keytpe, valtpe)}
          var i = 0
          while (i < length) {
            val r = arrReader.readElement()
            ${
              if (isPrimitive) q"""
                r.beginEntryNoTag()
                val elem = elunpickler.unpickle(eltag, r).asInstanceOf[$eltpe]
                r.endEntry()
                buffer += elem
              """.asInstanceOf[Tree] else q"""
                val elem = r.unpickle[$eltpe]
                buffer += elem
              """.asInstanceOf[Tree]
            }
            i += 1
          }
          ${
            if (isPrimitive) q"arrReader.unpinHints()".asInstanceOf[Tree]
            else q"".asInstanceOf[Tree]
          }
          arrReader.endCollection()
          ${mkResult(q"buffer")}
        }
      }
      $picklerUnpicklerName
    """
  }
}

trait CollectionPicklerUnpicklerMacro extends Macro {
  def mkType(eltpe: c.Type): c.Type
  def mkArray(picklee: c.Tree): c.Tree
  def mkBuffer(eltpe: c.Type): c.Tree
  def mkResult(buffer: c.Tree): c.Tree

  def impl[T: c.WeakTypeTag](format: c.Tree): c.Tree = {
    import c.universe._
    import definitions._
    val tpe = mkType(weakTypeOf[T])
    val eltpe = weakTypeOf[T]
    val isPrimitive = eltpe.isEffectivelyPrimitive
    val isFinal = eltpe.isEffectivelyFinal
    val picklerUnpicklerName = c.fresh(syntheticPicklerUnpicklerName(tpe).toTermName)
    q"""
      implicit object $picklerUnpicklerName extends scala.pickling.SPickler[$tpe] with scala.pickling.Unpickler[$tpe] {
        import scala.reflect.runtime.universe._
        import scala.pickling._
        import scala.pickling.`package`.PickleOps

        val format = implicitly[${format.tpe}]

        val elpickler: SPickler[$eltpe] = {
          val elpickler = "bam!"
          implicitly[SPickler[$eltpe]]
        }
        val elunpickler: Unpickler[$eltpe] = {
          val elunpickler = "bam!"
          implicitly[Unpickler[$eltpe]]
        }
        val eltag: scala.pickling.FastTypeTag[$eltpe] = {
          val eltag = "bam!"
          implicitly[scala.pickling.FastTypeTag[$eltpe]]
        }
        val colltag: scala.pickling.FastTypeTag[$tpe] = {
          val colltag = "bam!"
          implicitly[scala.pickling.FastTypeTag[$tpe]]
        }

        def pickle(picklee: $tpe, builder: PBuilder): Unit = {
          builder.hintTag(colltag)
          ${
            if (eltpe =:= IntTpe) q"builder.hintKnownSize(picklee.length * 4 + 100)".asInstanceOf[Tree]
            else q"".asInstanceOf[Tree]
          }
          builder.beginEntry(picklee)
          ${
            if (isPrimitive) q"builder.hintStaticallyElidedType(); builder.hintTag(eltag); builder.pinHints()".asInstanceOf[Tree]
            else q"".asInstanceOf[Tree]
          }
          val arr = ${mkArray(q"picklee")}
          val length = arr.length
          builder.beginCollection(arr.length)
          var i = 0
          while (i < arr.length) {
            builder putElement { b =>
              ${
                if (!isPrimitive && !isFinal) q"""
                  b.hintTag(eltag)
                  arr(i).pickleInto(b)
                """.asInstanceOf[Tree] else if (!isPrimitive && isFinal) q"""
                  b.hintTag(eltag)
                  b.hintStaticallyElidedType()
                  arr(i).pickleInto(b)
                """.asInstanceOf[Tree] else q"""
                  elpickler.pickle(arr(i), b)
                """.asInstanceOf[Tree]
              }
            }
            i += 1
          }
          ${
            if (isPrimitive) q"builder.unpinHints()".asInstanceOf[Tree]
            else q"".asInstanceOf[Tree]
          }
          builder.endCollection(i)
          builder.endEntry()
        }
        def unpickle(tag: => scala.pickling.FastTypeTag[_], reader: PReader): Any = {
          val arrReader = reader.beginCollection()
          ${
            if (isPrimitive) q"arrReader.hintStaticallyElidedType(); arrReader.hintTag(eltag); arrReader.pinHints()".asInstanceOf[Tree]
            else q"".asInstanceOf[Tree]
          }
          val length = arrReader.readLength()
          var buffer = ${mkBuffer(eltpe)}
          var i = 0
          while (i < length) {
            val r = arrReader.readElement()
            ${
              if (isPrimitive) q"""
                r.beginEntryNoTag()
                val elem = elunpickler.unpickle(eltag, r).asInstanceOf[$eltpe]
                r.endEntry()
                buffer += elem
              """.asInstanceOf[Tree] else q"""
                val elem = r.unpickle[$eltpe]
                buffer += elem
              """.asInstanceOf[Tree]
            }
            i += 1
          }
          ${
            if (isPrimitive) q"arrReader.unpinHints()".asInstanceOf[Tree]
            else q"".asInstanceOf[Tree]
          }
          arrReader.endCollection()
          ${mkResult(q"buffer")}
        }
      }
      $picklerUnpicklerName
    """
  }
}
