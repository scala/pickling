package scala.pickling.implicitparams

import org.scalatest.FunSuite
import scala.pickling._
import json._


case class Person(implicit name: String, age: Int)
object Test {
  class SimpleImplParamTest extends FunSuite {
    test("main") {
      implicit val nme = "Harry"
      implicit val age = 18

      val per = Person()
      val p = per.pickle
      val up = p.unpickle[Person]
      assert(per == up)
    }
  }
}
