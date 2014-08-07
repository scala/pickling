import scala.pickling._
import json._

// we need a custom pickler for Tuple2
// it's not implicit because we put it in the registry instead

  object Tuple2Pickler extends SPickler[Tuple2[_, _]] {
    val format = null // not used
    def pickle(picklee: Tuple2[_, _], builder: PBuilder ): Unit = {
      println("yay")
      val maintag = FastTypeTag("scala.Tuple2")
      builder.hintTag(maintag)
      builder.beginEntry(picklee)

      builder.putField("_1", b => {
        // we need to look up a pickler for _1
        val fldClass = if (picklee._1 == null) null else picklee._1.getClass
        val onePickler = SPickler.genPickler(picklee.getClass.getClassLoader, fldClass).asInstanceOf[SPickler[Any]]
        val tag = scala.pickling.FastTypeTag(if (fldClass == null) "Null" else fldClass.getName)
        b.hintTag(tag)
        // TODO: this is not always correct, need something like in Runtime.scala
        b.hintStaticallyElidedType()
        onePickler.pickle(picklee._1, b)
      })

      builder.putField("_2", b => {
        val two = picklee._2
        val fldClass = if (two == null) null else two.getClass
        val twoPickler = SPickler.genPickler(picklee.getClass.getClassLoader, fldClass).asInstanceOf[SPickler[Any]]
        val tag = scala.pickling.FastTypeTag(if (fldClass == null) "Null" else fldClass.getName)
        b.hintTag(tag)
        b.hintStaticallyElidedType()
        twoPickler.pickle(two, b)
      })

      builder.endEntry()
    }
  }

case class C(x: Int)
case class D(x: Int, y: Double)
case class E(x: Int, y: String)
case class F(x: Int, y: C)
case class G(a: Array[Int])

object hybrid extends App {

  // refer to SPickler object once, so it gets initialized (all implicit vals initialized)
  val init = SPickler

  // now inspect GlobalRegistry.picklerMap
  GlobalRegistry.picklerMap.foreach(println)
  println()

  def roundTrip(obj: Any): Unit = {
    val p = obj.pickle
    println(p.value)
    val up = p.unpickle[Any]
    assert(obj == up)
  }

  roundTrip(C(5))
  roundTrip(D(5, 6.2d))
  roundTrip(E(5, "hello"))
  roundTrip(F(6, C(5)))
  roundTrip(G(Array(1, 2, 3)))

/*
  val t: Any = ('c', "hello")
  println(s"getClass: ${t.getClass.getName}")

  def pureStatic(): Unit = {
    val ts: (Char, String) = ('c', "hello")
    val p = ts.pickle
    println(p.value)
    val up = p.unpickle[(Char, String)]
    println(up.toString)
  }

  def pureRuntime(): Unit = {
    val p = t.pickle
    println(p.value)
    val up = p.unpickle[Any]
    println(up.toString)
  }

  def fastRuntime(): Unit = {
    GlobalRegistry.picklerMap += ("scala.Tuple2" -> Tuple2Pickler)
    val p = t.pickle // should pick the one from the registry

    println(p.value)

    //val up = p.unpickle[Any]
    //println(up.toString)
  }

  //pureStatic() // works just fine
  //pureRuntime()

  val c: Any = C(5)
  println(c.getClass.getName)
  val p = c.pickle
  println(p.value)
*/
}
