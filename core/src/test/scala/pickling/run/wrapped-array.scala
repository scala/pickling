package scala.pickling.test.collection

import org.scalatest.FunSuite

import scala.pickling._, scala.pickling.Defaults._, json._
import runtime.GlobalRegistry

import scala.collection.mutable.WrappedArray

case class Rating(x: Int)

class WrappedArrayTest extends FunSuite {
  def mkAnyRefWrappedArrayPickler(implicit pf: PickleFormat):
    Pickler[WrappedArray.ofRef[AnyRef]] with Unpickler[WrappedArray.ofRef[AnyRef]] =
      new Pickler[WrappedArray.ofRef[AnyRef]] with Unpickler[WrappedArray.ofRef[AnyRef]] {

    val format: PickleFormat = pf

    val mirror = scala.reflect.runtime.currentMirror

    def tag: FastTypeTag[WrappedArray.ofRef[AnyRef]] = implicitly[FastTypeTag[WrappedArray.ofRef[AnyRef]]]

    def pickle(coll: WrappedArray.ofRef[AnyRef], builder: PBuilder): Unit = {
      builder.hintTag(implicitly[FastTypeTag[WrappedArray.ofRef[AnyRef]]])
      builder.beginEntry(coll)

      builder.beginCollection(coll.size)
      coll.foreach { (elem: AnyRef) =>
        builder putElement { b =>
          val elemClass = elem.getClass
          // TODO: allow passing in ClassLoader to picklers selected from registry
          val classLoader: ClassLoader = elemClass.getClassLoader
          val elemTag = FastTypeTag.mkRaw(elemClass, mirror) // slow: `mkRaw` is called for each element
          b.hintTag(elemTag)
          val pickler = runtime.RuntimePicklerLookup.genPickler(classLoader, elemClass, elemTag).asInstanceOf[Pickler[AnyRef]]
          pickler.pickle(elem, b)
        }
      }
      builder.endCollection()

      builder.endEntry()
    }

    def unpickle(tpe: String, preader: PReader): Any = {
      val reader = preader.beginCollection()

      val length = reader.readLength()
      val elemClass = (new Object).getClass
      val newArray = java.lang.reflect.Array.newInstance(elemClass, length).asInstanceOf[Array[AnyRef]]

      var i = 0
      while (i < length) {
        val r = reader.readElement()
        val elemTag = r.beginEntry()
        val elemUnpickler = runtime.RuntimeUnpicklerLookup.genUnpickler(mirror, elemTag)
        val elem = elemUnpickler.unpickle(elemTag, r)
        r.endEntry()
        newArray(i) = elem.asInstanceOf[AnyRef]
        i = i + 1
      }

      preader.endCollection()
      new WrappedArray.ofRef(newArray)
    }
  }

  GlobalRegistry.picklerMap += ("scala.collection.mutable.WrappedArray$ofRef" -> (x => mkAnyRefWrappedArrayPickler))
  GlobalRegistry.unpicklerMap += ("scala.collection.mutable.WrappedArray.ofRef[java.lang.Object]" -> mkAnyRefWrappedArrayPickler)

  test("main") {
    val l = List(Rating(10), Rating(5), Rating(2))

    val wa = new WrappedArray.ofRef(l.toArray)

    val o: Any = wa

    val p = o.pickle
    val up = p.unpickle[Any]

    assert(up.toString == "WrappedArray(Rating(10), Rating(5), Rating(2))")
  }
}
