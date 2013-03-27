import scala.pickling._
import binary._

class C[T]
case class D[T](x: T) extends C[T]

object Test extends App {
  val c: C[Int] = D(2)
  val p = c.pickle
  println(p)
  println(p.unpickle[C[Int]])
}
