package scala.pickling.test.sealedtraitinarray

import scala.pickling._
import scala.pickling.Defaults._
import scala.pickling.static._
import scala.pickling.json._

import org.scalatest.FunSuite

sealed trait Fruit
case class Apple(kind: String) extends Fruit
case class Orange(kind: String) extends Fruit

class SealedTraitInArrayTest extends FunSuite {
  test("sealed trait in Array") {
    // Type annotation is needed here otherwise you get Array[Product with Serializable with Fruit]
    val arr: Array[Fruit] = Array(Apple("x"), Orange("x"))
    val pkl = arr.pickle
    // println(pkl)
    val obj = pkl.unpickle[Array[Fruit]]
    assert(obj === arr)
  }
}
