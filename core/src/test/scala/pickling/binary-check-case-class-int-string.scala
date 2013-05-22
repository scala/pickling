package scala.pickling.test

import scala.pickling._
import binary._

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object BinaryCaseClassIntStringSpecification extends Properties("case class Int String") {

  case class Person(name: String, age: Int)

  property("pickle/unpickle") = forAll((name: String) => {
    val p = Person(name, 43)
    val pickle = p.pickle
    val up = pickle.unpickle[Person]
    p == up
  })

}
