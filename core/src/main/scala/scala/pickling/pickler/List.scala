package scala.pickling
package pickler

import scala.language.experimental.macros
import scala.language.higherKinds

trait ListPicklerUnpicklerMacro extends CollectionPicklerUnpicklerMacro {
  import c.universe._
  import definitions._
  lazy val ConsClass = c.mirror.staticClass("scala.collection.immutable.$colon$colon")
  def mkType(eltpe: c.Type) = appliedType(ConsClass.toTypeConstructor, List(eltpe))
  def mkArray(picklee: c.Tree) = q"$picklee.toArray"
  def mkBuffer(eltpe: c.Type) = q"_root_.scala.collection.mutable.ListBuffer[$eltpe]()"
  def mkResult(buffer: c.Tree) = q"$buffer.toList"
}

trait CollectionPicklerUnpicklerMacro extends Macro with UnpickleMacros {
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
      implicit object $picklerUnpicklerName extends _root_.scala.pickling.AbstractPicklerUnpickler[$tpe] {

        val elpickler: _root_.scala.pickling.Pickler[$eltpe] = {
          val elpickler = "bam!"
          _root_.scala.Predef.implicitly[_root_.scala.pickling.Pickler[$eltpe]]
        }
        val elunpickler: _root_.scala.pickling.Unpickler[$eltpe] = {
          val elunpickler = "bam!"
          _root_.scala.Predef.implicitly[_root_.scala.pickling.Unpickler[$eltpe]]
        }
        val eltag: _root_.scala.pickling.FastTypeTag[$eltpe] = {
          val eltag = "bam!"
          _root_.scala.Predef.implicitly[_root_.scala.pickling.FastTypeTag[$eltpe]]
        }
        val colltag: _root_.scala.pickling.FastTypeTag[$tpe] = {
          val colltag = "bam!"
          _root_.scala.Predef.implicitly[_root_.scala.pickling.FastTypeTag[$tpe]]
        }

        def pickle(picklee: $tpe, builder: _root_.scala.pickling.PBuilder): _root_.scala.Unit = {
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
                  _root_.scala.pickling.functions.pickleInto(arr(i), b)
                """.asInstanceOf[Tree] else if (!isPrimitive && isFinal) q"""
                  b.hintTag(eltag)
                  b.hintStaticallyElidedType()
                  _root_.scala.pickling.functions.pickleInto(arr(i), b)
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
          builder.endCollection()
          builder.endEntry()
        }
        def unpickle(tag: => _root_.scala.pickling.FastTypeTag[_], reader: _root_.scala.pickling.PReader): _root_.scala.Any = {
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
              """.asInstanceOf[Tree] else {
                val readerUnpickleTree = readerUnpickle(eltpe, newTermName("r"))
                q"""
                  val elem = $readerUnpickleTree
                  buffer += elem
                """.asInstanceOf[Tree]
              }
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
        def tag: _root_.scala.pickling.FastTypeTag[$tpe] = colltag
      }
      $picklerUnpicklerName
    """
  }
}
