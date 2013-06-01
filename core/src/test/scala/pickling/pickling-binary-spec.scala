package scala.pickling.test

import scala.pickling._
import binary._

import org.scalacheck.{Properties, Prop, Arbitrary, Gen}
import Gen._
import Arbitrary.arbitrary

object PicklingBinarySpec extends Properties("pickling-binary") {

  sealed abstract class Base
  final class C(s: String) extends Base { override def toString = "C" }
  final class D(i: Int) extends Base { override def toString = "D" }

  implicit val arbitraryBase: Arbitrary[Base] = Arbitrary[Base](
    oneOf(arbitrary[String].map(s => new C(s)),
          arbitrary[Int].map(i => new D(i))))

  sealed abstract class CaseBase
  case class CaseC(s: String) extends CaseBase
  case class CaseD(i: Int) extends CaseBase

  implicit val arbitraryCaseBase: Arbitrary[CaseBase] = Arbitrary[CaseBase](
    oneOf(arbitrary[String].map(CaseC(_)),
          arbitrary[Int].map(CaseD(_))))

  // subsumes test base.scala
  property("Base") = Prop forAll { (b: Base) =>
    val pickle: BinaryPickle = b.pickle
    val b1 = pickle.unpickle[Base]
    b1.toString == b.toString
  }

  property("CaseBase") = Prop forAll { (x: CaseBase) =>
    val pickle: BinaryPickle = x.pickle
    val x1 = pickle.unpickle[CaseBase]
    x1 == x
  }

  case class WithIntArray(a: Array[Int])

  implicit val arbitraryWithIntArray: Arbitrary[WithIntArray] =
    Arbitrary[WithIntArray](arbitrary[Array[Int]].map(WithIntArray(_)))

  property("case class with Array[Int] field") = Prop forAll { (x: WithIntArray) =>
    val pickle = x.pickle
    val x1 = pickle.unpickle[WithIntArray]
    x1 == x
    true
  }

  def emptyOrUnicode(s: String) =
    s == "" || s.exists(_.toInt > 255)

  /* The following two tests subsume test tuple2-primitive.scala */

  property("(String, Int)") = Prop forAll { (p: (String, Int)) =>
    if (emptyOrUnicode(p._1)) true //FIXME
    else {
      val pickle: BinaryPickle = p.pickle
      val p1 = pickle.unpickle[(String, Int)]
      p1 == p
    }
  }

  property("(String, Int, String)") = Prop forAll { (t: (String, Int, String)) =>
    if (emptyOrUnicode(t._1) || emptyOrUnicode(t._3)) true //FIXME
    else {
      val pickle: BinaryPickle = t.pickle
      val t1 = pickle.unpickle[(String, Int, String)]
      t1 == t
    }
  }

  property("(Int, (String, Int), Int)") = Prop forAll { (t: (Int, (String, Int), Int)) =>
    if (emptyOrUnicode(t._2._1)) true //FIXME
    else {
      val pickle: BinaryPickle = t.pickle
      val t1 = pickle.unpickle[(Int, (String, Int), Int)]
      t1 == t
    }
  }

  // subsumes test option-primitive.scala
  property("Option[Int]") = Prop forAll { (opt: Option[Int]) =>
    val pickle: BinaryPickle = opt.pickle
    val opt1 = pickle.unpickle[Option[Int]]
    opt1 == opt
  }

  property("Option[String]") = Prop forAll { (opt: Option[String]) =>
    val pickle: BinaryPickle = opt.pickle
    val opt1 = pickle.unpickle[Option[String]]
    opt1 == opt
  }

  property("(Option[String], String)") = Prop forAll { (t: (Option[String], String)) =>
    if (emptyOrUnicode(t._2)) true //FIXME
    else {
      val pickle: BinaryPickle = t.pickle
      val t1 = pickle.unpickle[(Option[String], String)]
      t1 == t
    }
  }

  property("Option[Option[Int]]") = Prop forAll { (opt: Option[Option[Int]]) =>
    val pickle: BinaryPickle = opt.pickle
    val opt1 = pickle.unpickle[Option[Option[Int]]]
    opt1 == opt
  }

  property("Option[CaseBase]") = Prop forAll { (opt: Option[CaseBase]) =>
    val pickle: BinaryPickle = opt.pickle
    val opt1 = pickle.unpickle[Option[CaseBase]]
    opt1 == opt
  }

  property("Int") = Prop forAll { (x: Int) =>
    val pickle: BinaryPickle = x.pickle
    val x1 = pickle.unpickle[Int]
    x1 == x
  }

  property("String") = Prop forAll { (x: String) =>
    val pickle: BinaryPickle = x.pickle
    val x1 = pickle.unpickle[String]
    x1 == x
  }

  property("Long") = Prop forAll { (x: Long) =>
    val pickle = x.pickle
    val x1 = pickle.unpickle[Long]
    x1 == x
  }

  property("Short") = Prop forAll { (x: Short) =>
    val pickle = x.pickle
    val x1 = pickle.unpickle[Short]
    x1 == x
  }

  property("Boolean") = Prop forAll { (x: Boolean) =>
    val pickle = x.pickle
    val x1 = pickle.unpickle[Boolean]
    x1 == x
  }
}
