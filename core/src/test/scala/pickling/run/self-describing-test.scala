package scala.pickling.runtime

import org.scalatest.FunSuite
import scala.pickling.{Ops, Pickler, Unpickler}
import scala.pickling.pickler.AllPicklers
import scala.pickling.binary.{BinaryFormats, BinaryPickleArray}
import scala.pickling.internal.{HybridRuntime}

abstract class SDPicklingLogic extends Ops with AllPicklers with BinaryFormats

object SDPicklingProtocol extends {
  val ignoreMe = scala.pickling.internal.replaceRuntime(new HybridRuntime)
} with SDPicklingLogic {
  implicit val so = scala.pickling.static.StaticOnly
}


final case class SelfDescribing(unpicklerClassName: String, blob: Array[Byte]) {
  import SDPicklingProtocol._

  def result(): Any = {
    val unpicklerInst = Class.forName(unpicklerClassName).newInstance().asInstanceOf[Unpickler[Any]]
    val pickle = BinaryPickleArray(blob)
    val reader = pickleFormat.createReader(pickle)
    val typeString = reader.beginEntry()
    reader.hintElidedType(unpicklerInst.tag)
    unpicklerInst.unpickle(unpicklerInst.tag.key, reader)
  }
}


sealed abstract class Message
case class Req[T](x: Int, y: T) extends Message
case class Foo[B](b: B)


class SelfDescribingTest extends FunSuite {

  test("SelfDescribing should work") {

    import SDPicklingProtocol._

    val pickler = implicitly[Pickler[Foo[Req[List[String]]]]]
    val unpickler = implicitly[Unpickler[Foo[Req[List[String]]]]]

    println(s"found pickler of class ${pickler.getClass.getName}")
    println(s"found unpickler of class ${unpickler.getClass.getName}")

    val req = Foo(Req(5, List("hello")))
    val p = req.pickle
    val sd = SelfDescribing(unpickler.getClass.getName, p.value)
    val sdp = sd.pickle

    val up = sdp.unpickle[SelfDescribing]
    val res = up.result()
    println(res)

    assert(res === req)

  }

}

