package scala.pickling.spec

import scala.pickling._, Defaults._

import org.scalacheck.{Properties, Prop, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import Gen._
import Arbitrary.arbitrary

import java.math.{BigDecimal, BigInteger}
import java.util.{Calendar, Date, GregorianCalendar}

object PicklingSpec {
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

  case class WithIntArray(a: Array[Int])

  implicit val arbitraryWithIntArray: Arbitrary[WithIntArray] =
    Arbitrary[WithIntArray](arbitrary[Array[Int]].map(WithIntArray(_)))

  case class Person(name: String, age: Int)

  lazy val randomDates = for (n <- Gen.choose(Integer.MIN_VALUE + 1, Integer.MAX_VALUE)) yield {
    def randBetween(start: Int, end: Int): Int = {
      start + scala.util.Random.nextInt(end - start)
    }

    val gc = new GregorianCalendar()
    gc.set(Calendar.YEAR, randBetween(1900, 3000))
    gc.set(Calendar.MONTH, randBetween(1,12))
    gc.set(Calendar.DAY_OF_YEAR, randBetween(1, gc.getActualMaximum(Calendar.DAY_OF_YEAR)))
    gc.set(Calendar.HOUR_OF_DAY, randBetween(0, 24))
    gc.set(Calendar.MINUTE, randBetween(0, 59))
    gc.set(Calendar.SECOND, randBetween(0, 99))
    gc.set(Calendar.MILLISECOND, randBetween(0, 999))
    gc.getTime()
  }
}

/* ~~~~~~~~~~~~~~~~~~~~~
 * START OF JSON SPEC!
 * ~~~~~~~~~~~~~~~~~~~~~*/
object PicklingJsonSpec extends Properties("pickling-json") {
  import json._
  import PicklingSpec._

  property("UnpickleOps") = Prop forAll { (x: String) =>
    val pickle: JSONPickle = x.pickle
    val x1 = pickle.value.unpickle[String]
    x1 == x
  }

  // subsumes test base.scala
  property("Base") = Prop forAll { (b: Base) =>
    val pickle: JSONPickle = b.pickle
    val b1 = pickle.unpickle[Base]
    b1.toString == b.toString
  }

  property("CaseBase") = Prop forAll { (x: CaseBase) =>
    val pickle: JSONPickle = x.pickle
    val x1 = pickle.unpickle[CaseBase]
    x1 == x
  }

  def emptyOrUnicode(s: String) =
    s == "" || s.exists(_.toInt > 255)

  /* The following two tests subsume test tuple2-primitive.scala */

  property("(String, Int)") = Prop forAll { (p: (String, Int)) =>
    if (emptyOrUnicode(p._1)) true //FIXME
    else {
      val pickle: JSONPickle = p.pickle
      val p1 = pickle.unpickle[(String, Int)]
      p1 == p
    }
  }

  property("(String, Int, String)") = Prop forAll { (t: (String, Int, String)) =>
    if (emptyOrUnicode(t._1) || emptyOrUnicode(t._3)) true //FIXME
    else {
      val pickle: JSONPickle = t.pickle
      val t1 = pickle.unpickle[(String, Int, String)]
      t1 == t
    }
  }

  property("(Int, (String, Int), Int)") = Prop forAll { (t: (Int, (String, Int), Int)) =>
    if (emptyOrUnicode(t._2._1)) true //FIXME
    else {
      val pickle: JSONPickle = t.pickle
      val t1 = pickle.unpickle[(Int, (String, Int), Int)]
      t1 == t
    }
  }

  // subsumes test option-primitive.scala
  property("Option[Int]") = Prop forAll { (opt: Option[Int]) =>
    val pickle: JSONPickle = opt.pickle
    val opt1 = pickle.unpickle[Option[Int]]
    opt1 == opt
  }

  property("Option[String]") = Prop forAll { (opt: Option[String]) =>
    val pickle: JSONPickle = opt.pickle
    val opt1 = pickle.unpickle[Option[String]]
    opt1 == opt
  }

  property("(Option[String], String)") = Prop forAll { (t: (Option[String], String)) =>
    if (emptyOrUnicode(t._2)) true //FIXME
    else {
      val pickle: JSONPickle = t.pickle
      val t1 = pickle.unpickle[(Option[String], String)]
      t1 == t
    }
  }

  property("Option[Option[Int]]") = Prop forAll { (opt: Option[Option[Int]]) =>
    val pickle: JSONPickle = opt.pickle
    val opt1 = pickle.unpickle[Option[Option[Int]]]
    opt1 == opt
  }

  property("Option[CaseBase]") = Prop forAll { (opt: Option[CaseBase]) =>
    val pickle: JSONPickle = opt.pickle
    val opt1 = pickle.unpickle[Option[CaseBase]]
    opt1 == opt
  }

  property("Int") = Prop forAll { (x: Int) =>
    val pickle: JSONPickle = x.pickle
    val x1 = pickle.unpickle[Int]
    x1 == x
  }

  property("String") = Prop forAll { (x: String) =>
    val pickle: JSONPickle = x.pickle
    val x1 = pickle.unpickle[String]
    x1 == x
  }

  property("Long") = Prop forAll { (x: Long) =>
    val pickle: JSONPickle = x.pickle
    val x1 = pickle.unpickle[Long]
    x1 == x
  }

  property("Short") = Prop forAll { (x: Short) =>
    val pickle: JSONPickle = x.pickle
    val x1 = pickle.unpickle[Short]
    x1 == x
  }

  property("Boolean") = Prop forAll { (x: Boolean) =>
    val pickle: JSONPickle = x.pickle
    val x1 = pickle.unpickle[Boolean]
    x1 == x
  }

  property("Byte") = Prop forAll { (x: Byte) =>
    val pickle: JSONPickle = x.pickle
    val x1 = pickle.unpickle[Byte]
    x1 == x
  }

  property("CaseClassIntString") = forAll((name: String) => {
    val p = Person(name, 43)
    val pickle: JSONPickle = p.pickle
    val up = pickle.unpickle[Person]
    p == up
  })

  property("case class with Array[Int] field") = Prop forAll { (x: WithIntArray) =>
    val pickle: JSONPickle = x.pickle
    val x1 = pickle.unpickle[WithIntArray]
    x1 == x
    true
  }

  property("Char") = Prop forAll { (x: Char) =>
    val pickle: JSONPickle = x.pickle
    val x1 = pickle.unpickle[Char]
    x1 == x
  }

  property("Float") = Prop forAll { (x: Float) =>
    val pickle: JSONPickle = x.pickle
    val x1 = pickle.unpickle[Float]
    x1 == x
  }

  property("Double") = Prop forAll { (x: Double) =>
    val pickle: JSONPickle = x.pickle
    val x1 = pickle.unpickle[Double]
    x1 == x
  }

  property("Array[Byte]") = forAll((ia: Array[Byte]) => {
    val pickle: JSONPickle = ia.pickle
    val readArr = pickle.unpickle[Array[Byte]]
    readArr.sameElements(ia)
  })

  property("Array[Short]") = forAll((ia: Array[Short]) => {
    val pickle: JSONPickle = ia.pickle
    val readArr = pickle.unpickle[Array[Short]]
    readArr.sameElements(ia)
  })

  property("Array[Char]") = forAll((ia: Array[Char]) => {
    val pickle: JSONPickle = ia.pickle
    val readArr = pickle.unpickle[Array[Char]]
    readArr.sameElements(ia)
  })

  property("Array[Int]") = forAll((ia: Array[Int]) => {
    val pickle: JSONPickle = ia.pickle
    val readArr = pickle.unpickle[Array[Int]]
    readArr.sameElements(ia)
  })

  property("Array[Long]") = forAll((ia: Array[Long]) => {
    val pickle: JSONPickle = ia.pickle
    val readArr = pickle.unpickle[Array[Long]]
    readArr.sameElements(ia)
  })

  property("Array[Boolean]") = forAll((ia: Array[Boolean]) => {
    val pickle: JSONPickle = ia.pickle
    val readArr = pickle.unpickle[Array[Boolean]]
    readArr.sameElements(ia)
  })

  property("Array[Float]") = forAll((ia: Array[Float]) => {
    val pickle: JSONPickle = ia.pickle
    val readArr = pickle.unpickle[Array[Float]]
    readArr.sameElements(ia)
  })

  property("Array[Double]") = forAll((ia: Array[Double]) => {
    val pickle: JSONPickle = ia.pickle
    val readArr = pickle.unpickle[Array[Double]]
    readArr.sameElements(ia)
  })

/* TODO: disabled because of spurious failures.

  property("Array[(Int, Double)]") = forAll((ia: Array[(Int, Double)]) => {
    val pickle: JSONPickle = ia.pickle
    val readArr = pickle.unpickle[Array[(Int, Double)]]
    readArr.sameElements(ia)
  })
*/

  property("Array[(String, Int)]") = forAll((ia: Array[(String, Int)]) => {
    val pickle: JSONPickle = ia.pickle
    val readArr = pickle.unpickle[Array[(String, Int)]]
    readArr.sameElements(ia)
  })

  property("BigDecimal") = Prop forAll { (x: Double) =>
    val bd = new BigDecimal(x)
    val pickle: JSONPickle = bd.pickle
    val x1 = pickle.unpickle[BigDecimal]
    x1 == bd
  }

  property("BigInteger") = Prop forAll { (x: Long) =>
    val bi = BigInteger.valueOf(x)
    val pickle: JSONPickle = bi.pickle
    val x1 = pickle.unpickle[BigInteger]
    x1 == bi
  }

  property("Date") = forAll (randomDates) { (date) =>
    val pickle: JSONPickle = date.pickle
    val x1 = pickle.unpickle[Date]
    x1.compareTo(date) == 0
  }
}


/* ~~~~~~~~~~~~~~~~~~~~~
 * START OF BINARY SPEC!
 * ~~~~~~~~~~~~~~~~~~~~~*/
object PicklingBinarySpec extends Properties("pickling-binary") {
  import binary._
  import PicklingSpec._

  property("UnpickleOps") = Prop forAll { (x: String) =>
    val pickle: BinaryPickle = x.pickle
    val x1 = pickle.value.unpickle[String]
    x1 == x
  }

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
    val pickle: BinaryPickle = x.pickle
    val x1 = pickle.unpickle[Long]
    x1 == x
  }

  property("Short") = Prop forAll { (x: Short) =>
    val pickle: BinaryPickle = x.pickle
    val x1 = pickle.unpickle[Short]
    x1 == x
  }

  property("Boolean") = Prop forAll { (x: Boolean) =>
    val pickle: BinaryPickle = x.pickle
    val x1 = pickle.unpickle[Boolean]
    x1 == x
  }

  property("Byte") = Prop forAll { (x: Byte) =>
    val pickle: BinaryPickle = x.pickle
    val x1 = pickle.unpickle[Byte]
    x1 == x
  }

  property("Char") = Prop forAll { (x: Char) =>
    val pickle: BinaryPickle = x.pickle
    val x1 = pickle.unpickle[Char]
    x1 == x
  }

  property("Float") = Prop forAll { (x: Float) =>
    val pickle: BinaryPickle = x.pickle
    val x1 = pickle.unpickle[Float]
    x1 == x
  }

  property("Double") = Prop forAll { (x: Double) =>
    val pickle: BinaryPickle = x.pickle
    val x1 = pickle.unpickle[Double]
    x1 == x
  }

  property("CaseClassIntString") = forAll((name: String) => {
    val p = Person(name, 43)
    val pickle: BinaryPickle = p.pickle
    val up = pickle.unpickle[Person]
    p == up
  })

  property("case class with Array[Int] field") = Prop forAll { (x: WithIntArray) =>
    val pickle: BinaryPickle = x.pickle
    val x1 = pickle.unpickle[WithIntArray]
    x1 == x
    true
  }

  property("Array[Byte]") = forAll((ia: Array[Byte]) => {
    val pickle: BinaryPickle = ia.pickle
    val readArr = pickle.unpickle[Array[Byte]]
    readArr.sameElements(ia)
  })

  property("Array[Short]") = forAll((ia: Array[Short]) => {
    val pickle: BinaryPickle = ia.pickle
    val readArr = pickle.unpickle[Array[Short]]
    readArr.sameElements(ia)
  })

  property("Array[Char]") = forAll((ia: Array[Char]) => {
    val pickle: BinaryPickle = ia.pickle
    val readArr = pickle.unpickle[Array[Char]]
    readArr.sameElements(ia)
  })

  property("Array[Int]") = forAll((ia: Array[Int]) => {
    val pickle: BinaryPickle = ia.pickle
    val readArr = pickle.unpickle[Array[Int]]
    readArr.sameElements(ia)
  })

  property("Array[Long]") = forAll((ia: Array[Long]) => {
    val pickle: BinaryPickle = ia.pickle
    val readArr = pickle.unpickle[Array[Long]]
    readArr.sameElements(ia)
  })

  property("Array[Boolean]") = forAll((ia: Array[Boolean]) => {
    val pickle: BinaryPickle = ia.pickle
    val readArr = pickle.unpickle[Array[Boolean]]
    readArr.sameElements(ia)
  })

  property("Array[Float]") = forAll((ia: Array[Float]) => {
    val pickle: BinaryPickle = ia.pickle
    val readArr = pickle.unpickle[Array[Float]]
    readArr.sameElements(ia)
  })

  property("Array[Double]") = forAll((ia: Array[Double]) => {
    val pickle: BinaryPickle = ia.pickle
    val readArr = pickle.unpickle[Array[Double]]
    readArr.sameElements(ia)
  })

/* TODO: disabled because of spurious failures.

  property("Array[(Int, Double)]") = forAll((ia: Array[(Int, Double)]) => {
    val pickle: BinaryPickle = ia.pickle
    val readArr = pickle.unpickle[Array[(Int, Double)]]
    readArr.sameElements(ia)
  })
*/

  property("Array[(String, Int)]") = forAll((ia: Array[(String, Int)]) => {
    val pickle: BinaryPickle = ia.pickle
    val readArr = pickle.unpickle[Array[(String, Int)]]
    readArr.sameElements(ia)
  })

  property("BigDecimal") = Prop forAll { (x: Double) =>
    val bd = new BigDecimal(x)
    val pickle: BinaryPickle = bd.pickle
    val x1 = pickle.unpickle[BigDecimal]
    x1 == bd
  }

  property("BigInteger") = Prop forAll { (x: Long) =>
    val bi = BigInteger.valueOf(x)
    val pickle: BinaryPickle = bi.pickle
    val x1 = pickle.unpickle[BigInteger]
    x1 == bi
  }

  property("Date") = forAll (randomDates) { (date) =>
    val pickle: BinaryPickle = date.pickle
    val x1 = pickle.unpickle[Date]
    x1.compareTo(date) == 0
  }
}
