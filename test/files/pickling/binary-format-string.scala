import scala.pickling._
import binary._
import reflect.runtime.{universe => ru}
import ru._

case class Person(name: String)

object Test extends App {

  val pf = new BinaryPickleFormat
  val pb = pf.createBuilder()

  val p = new Person("Jim")

  pb.beginEntry(typeOf[Person], p)
  pb.putField("name", b => {
    b.beginEntry(typeOf[String], "Jim")
    b.endEntry()
  })
  pb.endEntry()
  val res: BinaryPickle = pb.result()

  val arr = res.value
  println("ARRAY:")
  println(arr.mkString("[", ",", "]"))
  
  val rtm = ru.runtimeMirror(getClass.getClassLoader)
  val pr = pf.createReader(res)
  val tpe = pr.readType(rtm)
  println(s"TYPE: [$tpe]")

  val pr2 = pr.readField("name")

  val tpe2 = pr2.readType(rtm)
  println(s"TYPE: [$tpe2]")

  val primValue = pr2.readPrimitive(typeOf[String]).asInstanceOf[String]
  val up = new Person(primValue)
  println("unpickled: " + up)
}
