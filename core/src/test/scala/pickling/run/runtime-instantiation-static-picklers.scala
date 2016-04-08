package scala.pickling.runtime

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

case class Container[T](value: T)

object RuntimeHelper {
  /* Instantiate a class for an already generated `Unpickler` */
  def getInstance[T](className: String): T = {
    try {
      val clazz = Class.forName(className)
      println(s"Create instance of $clazz")
      clazz.newInstance().asInstanceOf[T]
    } catch {
      case ex: Throwable =>
        scala.concurrent.util.Unsafe.instance.allocateInstance(
          Class.forName(className)
        ).asInstanceOf[T]
    }
  }
}

class StaticJavaRuntime extends FunSuite {

  implicit val staticOnly = static.StaticOnly

  test("runtime instantiated pickler should work") {

    val c = Container(List(1,2,3,4,5))
    val containerPickler = implicitly[Pickler[Container[List[Int]]]]
    val pickled = c.pickle.value

    val picklerClassName = containerPickler.getClass.getName
    val runtimePickler = RuntimeHelper.getInstance[Pickler[Container[Any]]](picklerClassName)
    val format = implicitly[PickleFormat]
    val unknownContainer = c.asInstanceOf[Container[Any]]
    val pickled2 = runtimePickler.pickle(unknownContainer, format.createBuilder())
    println(pickled)
    println(pickled2)

    assert(pickled === pickled2)

  }

  test("runtime instantiated unpickler should work") {

    val c = Container(List(1,2,3,4,5))
    val pickled = c.pickle.value
    val blob = JSONPickle(pickled)
    val reader = pickleFormat.createReader(blob)

    val containerUnpickler = implicitly[Unpickler[Container[List[Int]]]]
    val unpicklerClassName = containerUnpickler.getClass.getName
    val runtimeUnpickler = RuntimeHelper.getInstance[Unpickler[Container[Any]]](unpicklerClassName)
    
    // Very important, assume it's elided
    val tag = runtimeUnpickler.tag
    assert(tag != null)

    val unpickled = runtimeUnpickler.unpickle(tag.key, reader)
    assert(c === unpickled)

  }

}

object ContainerPickling {

  implicit def containerPicklerUnpickler[T: FastTypeTag]
    (implicit p: Pickler[T], u: Unpickler[T]): Pickler[Container[T]] with Unpickler[Container[T]] =
      new Pickler[Container[T]] with Unpickler[Container[T]] {
        def tag = implicitly[FastTypeTag[Container[T]]]

        def pickle(picklee: Container[T], builder: PBuilder) = {

          builder.beginEntry(picklee, tag)
          builder.putField("value", {b =>
            b.hintElidedType(p.tag)
            p.pickle(picklee.value, b)
          })
          builder.endEntry()

        }

        def unpickle(tag: String, reader: PReader) = {

          val reader1 = reader.readField("value")
          reader1.hintElidedType(u.tag)
          val tag1 = reader1.beginEntry()
          val result = u.unpickle(tag1, reader1).asInstanceOf[T]
          reader1.endEntry()

        }
      }

  implicit def containerPicklerUnpicklerImplicitly[T: FastTypeTag]
    (implicit p: Pickler[T], u: Unpickler[T]): Pickler[Container[T]] with Unpickler[Container[T]] =
      new Pickler[Container[T]] with Unpickler[Container[T]] {
        def tag = implicitly[FastTypeTag[Container[T]]]

        def pickle(picklee: Container[T], builder: PBuilder) = {

          builder.beginEntry(picklee, tag)
          builder.putField("value", {b =>
            b.hintElidedType(implicitly[FastTypeTag[T]])
            // Ask directly to the scope instead of relying on `p`
            val pickler = implicitly[Pickler[T]]
            pickler.pickle(picklee.value, b)
          })
          builder.endEntry()

        }

        def unpickle(tag: String, reader: PReader) = {

          val reader1 = reader.readField("value")
          reader1.hintElidedType(implicitly[FastTypeTag[T]])
          val tag1 = reader1.beginEntry()
            // Ask directly to the scope instead of relying on `u`
          val unpickler = implicitly[Unpickler[T]]
          val result = unpickler.unpickle(tag1, reader1).asInstanceOf[T]
          reader1.endEntry()

        }
      }

}

class StaticJavaRuntimeCustomPickler extends FunSuite {

  implicit val staticOnly = static.StaticOnly
  import ContainerPickling.containerPicklerUnpickler

  test("runtime instantiated custom pickler should work") {

    val c = Container(List(1,2,3,4,5))
    val containerPickler = containerPicklerUnpickler[List[Int]]
    val pickled = c.pickle.value

    val picklerClassName = containerPickler.getClass.getName
    val runtimePickler = RuntimeHelper.getInstance[Pickler[Container[Any]]](picklerClassName)
    val format = implicitly[PickleFormat]
    val unknownContainer = c.asInstanceOf[Container[Any]]
    val pickled2 = runtimePickler.pickle(unknownContainer, format.createBuilder())
    println(pickled)
    println(pickled2)

    assert(pickled === pickled2)

  }

  test("runtime instantiated custom unpickler should work") {

    val c = Container(List(1,2,3,4,5))
    val pickled = c.pickle.value
    val blob = JSONPickle(pickled)
    val reader = pickleFormat.createReader(blob)

    val containerUnpickler = containerPicklerUnpickler[List[Int]]
    val unpicklerClassName = containerUnpickler.getClass.getName
    val runtimeUnpickler = RuntimeHelper.getInstance[Unpickler[Container[Any]]](unpicklerClassName)
    
    // Very important, assume it's elided
    val tag = runtimeUnpickler.tag
    assert(tag != null)

    val unpickled = runtimeUnpickler.unpickle(tag.key, reader)
    assert(c === unpickled)

  }

}

class StaticJavaRuntimeCustomPicklerImplicitly extends FunSuite {

  implicit val staticOnly = static.StaticOnly
  import ContainerPickling.containerPicklerUnpicklerImplicitly

  test("runtime instantiated custom pickler calling implicitly should work") {

    val c = Container(List(1,2,3,4,5))
    val containerPickler = containerPicklerUnpicklerImplicitly[List[Int]]
    val pickled = c.pickle.value

    val picklerClassName = containerPickler.getClass.getName
    val runtimePickler = RuntimeHelper.getInstance[Pickler[Container[Any]]](picklerClassName)
    val format = implicitly[PickleFormat]
    val unknownContainer = c.asInstanceOf[Container[Any]]
    val pickled2 = runtimePickler.pickle(unknownContainer, format.createBuilder())
    println(pickled)
    println(pickled2)

    assert(pickled === pickled2)

  }

  test("runtime instantiated custom unpickler calling implicitly should work") {

    val c = Container(List(1,2,3,4,5))
    val pickled = c.pickle.value
    val blob = JSONPickle(pickled)
    val reader = pickleFormat.createReader(blob)

    val containerUnpickler = containerPicklerUnpicklerImplicitly[List[Int]]
    val unpicklerClassName = containerUnpickler.getClass.getName
    val runtimeUnpickler = RuntimeHelper.getInstance[Unpickler[Container[Any]]](unpicklerClassName)
    
    // Very important, assume it's elided
    val tag = runtimeUnpickler.tag
    assert(tag != null)

    val unpickled = runtimeUnpickler.unpickle(tag.key, reader)
    assert(c === unpickled)

  }

}

