import scala.pickling._
import binary._

object Test extends App {
  val opt = Some(9)
  val pckl = opt.pickle
  println(pckl.unpickle[Option[Int]])
}