package scala.pickling.runtime.spec

import scala.pickling._, Defaults._
import org.scalacheck.{Properties, Prop, Gen}
import Gen._

case class C(x: Int)
case class D(x: Int, y: Double)
case class E(x: Int, y: String)
case class F(x: Int, y: C)
case class G(a: Array[Int])

object RuntimeJsonSpec extends Properties("runtime-json") {
  import json._

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

  property("(Int, String)") = Prop forAll { (p: (Int, String)) =>
    roundTrip(p)
  }

  property("C") = Prop forAll { (i: Int) =>
    roundTrip(C(i))
  }

  property("D") = Prop forAll { (p: (Int, Double)) =>
    roundTrip(D(p._1, p._2))
  }

  property("E") = Prop forAll { (p: (Int, String)) =>
    roundTrip(E(p._1, p._2))
  }

  property("F") = Prop forAll { (p: (Int, Int)) =>
    roundTrip(F(p._1, C(p._2)))
  }

  property("G") = Prop forAll { (arr: Array[Int]) =>
    roundTripG(G(arr))
  }

  property("(Int, Array[Double])") = Prop forAll { (t: (Int, Array[Double])) =>
    val obj: Any = t
    val p = obj.pickle
    val up = p.unpickle[Any]
    val t2 = up.asInstanceOf[(Int, Array[Double])]
    t._1 == t2._1 && t._2.mkString(",") == t2._2.mkString(",")
  }
}

object RuntimeBinarySpec extends Properties("runtime-binary") {
  import binary._

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

  property("(Int, String)") = Prop forAll { (p: (Int, String)) =>
    roundTrip(p)
  }

  property("C") = Prop forAll { (i: Int) =>
    roundTrip(C(i))
  }

  property("D") = Prop forAll { (p: (Int, Double)) =>
    roundTrip(D(p._1, p._2))
  }

  property("E") = Prop forAll { (p: (Int, String)) =>
    roundTrip(E(p._1, p._2))
  }

  property("F") = Prop forAll { (p: (Int, Int)) =>
    roundTrip(F(p._1, C(p._2)))
  }

  property("G") = Prop forAll { (arr: Array[Int]) =>
    roundTripG(G(arr))
  }

  property("(Int, Array[Double])") = Prop forAll { (t: (Int, Array[Double])) =>
    val obj: Any = t
    val p = obj.pickle
    val up = p.unpickle[Any]
    val t2 = up.asInstanceOf[(Int, Array[Double])]
    t._1 == t2._1 && t._2.mkString(",") == t2._2.mkString(",")
  }
}
