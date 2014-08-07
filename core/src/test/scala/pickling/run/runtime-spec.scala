package scala.runtime.spec

import scala.pickling._

import org.scalacheck.{Properties, Prop, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import Gen._

// object RuntimeSpec {
case class C(x: Int)
case class D(x: Int, y: Double)
case class E(x: Int, y: String)
case class F(x: Int, y: C)
case class G(a: Array[Int])
// }

object RuntimeJsonSpec extends Properties("runtime-json") {
  import json._
  // import RuntimeSpec._

  // there's no way to actually use any of our testing frameworks
  // to guarantee that the runtime picklers are being used
  // though by calling pickle on something that's statically
  // resolved to be Any will always trigger runtime pickling
  def roundTrip(obj: Any): Boolean = {
    val p = obj.pickle
    val up = p.unpickle[Any]
    obj == up
  }

  // there's no way to actually use any of our testing frameworks
  // to guarantee that the runtime picklers are being used
  // though by calling pickle on something that's statically
  // resolved to be Any will always trigger runtime pickling
  def roundTripG(obj: Any): Boolean = {
    val p = obj.pickle
    val up = p.unpickle[Any]
    obj.asInstanceOf[G].a.mkString(",") == up.asInstanceOf[G].a.mkString(",")
  }

  property("Int") = Prop forAll { (i: Int) =>
    roundTrip(i)
  }

  property("Double") = Prop forAll { (d: Double) =>
    roundTrip(d)
  }

  property("Long") = Prop forAll { (l: Long) =>
    roundTrip(l)
  }

  property("Char") = Prop forAll { (c: Char) =>
    roundTrip(c)
  }

  property("Float") = Prop forAll { (f: Float) =>
    roundTrip(f)
  }

  property("Boolean") = Prop forAll { (b: Boolean) =>
    roundTrip(b)
  }

  property("Short") = Prop forAll { (s: Short) =>
    roundTrip(s)
  }

  property("Byte") = Prop forAll { (b: Byte) =>
    roundTrip(b)
  }

  // It seems like Unit doesn't make sense to send to forAll?
  // property("Unit") = Prop forAll { (u: Unit) =>
  //   roundTrip(u)
  // }

  property("(Int, String))") = Prop forAll { (p: (Int, String)) =>
    roundTrip(p)
  }

  property("C[Int]") = Prop forAll { (i: Int) =>
    roundTrip(C(i))
  }

  property("D[(Int, Double)]") = Prop forAll { (p: (Int, Double)) =>
    roundTrip(D(p._1, p._2))
  }

  property("E[(Int, String)]") = Prop forAll { (p: (Int, String)) =>
    roundTrip(E(p._1, p._2))
  }

  property("F[Int, C[Int]]") = Prop forAll { (p: (Int, Int)) =>
    roundTrip(F(p._1, C(p._2)))
  }

  property("G[Array[Int]]") = Prop forAll { (arr: Array[Int]) =>
    roundTripG(G(arr))
  }
}