package scala.pickling.test

import scala.pickling._
import binary._

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object BinaryArrayIntSpecification extends Properties("Array[Int]") {

  property("pickle/unpickle") = forAll((ia: Array[Int]) => {
    val pickle: BinaryPickle = ia.pickle
    val readArr = pickle.unpickle[Array[Int]]
    readArr.sameElements(ia)
  })

}
