import scala.pickling._
import binary._
import reflect.runtime.{universe => ru}
import ru._

case class Person(age: Int)

object Test extends App {

  val pf = new BinaryPickleFormat
  val pb = pf.createBuilder()

  val p = new Person(41)

  pb.beginEntry(typeTag[Person], p)
  pb.putField("age", b => {
    b.beginEntry(typeTag[Int], 41)
    b.endEntry()
  })
  pb.endEntry()
  val res: BinaryPickle = pb.result()

  val arr = res.value
  println("ARRAY:")
  println(arr.mkString("[", ",", "]"))

  val rtm = ru.runtimeMirror(getClass.getClassLoader)
  val pr = pf.createReader(res)
  val tpe = pr.readTag(rtm).tpe
  println(s"TYPE: [$tpe]")

  val pr2 = pr.readField("age")

  val tpe2 = pr2.readTag(rtm).tpe
  println(s"TYPE: [$tpe2]")

  val intValue = pr2.readPrimitive(typeTag[Int]).asInstanceOf[Int]
  val up = new Person(intValue)
  println("unpickled: " + up)
}
