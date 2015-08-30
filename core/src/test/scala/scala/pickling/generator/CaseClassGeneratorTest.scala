package scala.pickling.generator

import org.scalatest.FunSuite

import scala.pickling._

/**
 * Tests the case class generator
 */
class CaseClassGeneratorTest extends FunSuite {
  import scala.pickling.Defaults._
  import scala.pickling.json._
  test("noConstructor") {
    implicit val pu = PicklingMacros.genPicklerUnpickler[CaseClassNoConstructor]
    val x = CaseClassNoConstructor()
    val y = x.pickle.unpickle[CaseClassNoConstructor]
    assert(x == y)
  }

  test("simple") {
    implicit val pu = PicklingMacros.genPicklerUnpickler[SimpleCaseClass]
    val x = SimpleCaseClass(1, "hi")
    val y = x.pickle.unpickle[SimpleCaseClass]
    assert(x == y)
  }

  /* NOTE - This will not work due to https://issues.scala-lang.org/browse/SI-9425
  test("privateConstructor") {
    val x = PrivateConstructorCaseClass(1, "hi")
    implicit val pu = scala.pickling.functions.testNewThing2[PrivateConstructorCaseClass]
    val y = x.pickle.unpickle[PrivateConstructorCaseClass]
    assert(x == y)
  }
  */


  test("mulitpleParamList") {
    implicit val pu = PicklingMacros.genPicklerUnpickler[MultipleParamListCaseClass]
    val x = MultipleParamListCaseClass(1)("hi")
    val y = x.pickle.unpickle[MultipleParamListCaseClass]
    assert(x == y)
    assert(x.y == y.y)
  }

  test("nestedVar") {
    implicit val pu = PicklingMacros.genPicklerUnpickler[NestedVarCaseClass]
    val x = NestedVarCaseClass(1)
    x.y = 2
    val y = x.pickle.unpickle[NestedVarCaseClass]
    assert(x == y)
    assert(x.y == y.y)
  }

  test("nestedCaseClass") {
    val outer = new NestedCaseClassHolder()
    val x = outer.NestedCaseClass(5)
    val y = x.pickle.unpickle[outer.NestedCaseClass]
    assert(x == y)
  }
  test("nestedValCaseClass") {
    implicit val pu = PicklingMacros.genPicklerUnpickler[NestedValCaseClass]
    val x = NestedValCaseClass(5)
    val y = x.pickle.unpickle[NestedValCaseClass]
    assert(x == y)
  }
  test("protectedMember") {
    implicit val pu = PicklingMacros.genPicklerUnpickler[ProtectedMemberCaseClass]
    val x = ProtectedMemberCaseClass(5, "hi")
    val y = x.pickle.unpickle[ProtectedMemberCaseClass]
    assert(x == y)
  }
  // NOTE: Old pickling aglorithm fails on this case
  test("privateMember") {
    implicit val pu = PicklingMacros.genPicklerUnpickler[PrivateMemberCaseClass ]
    val x = PrivateMemberCaseClass(5, "hi")
    val y = x.pickle.unpickle[PrivateMemberCaseClass]
    assert(x == y)
  }
  test("nestedPrivateVal") {
    implicit val pu = PicklingMacros.genPicklerUnpickler[NestedPrivateVarCaseClass]
    val x = NestedPrivateVarCaseClass(1)
    val y = x.pickle.unpickle[NestedPrivateVarCaseClass]
    assert(x == y)
  }
  test("nestedPrivateThisVar") {
    implicit val pu = PicklingMacros.genPicklerUnpickler[NestedPrivateThisCaseClass]
    val x = NestedPrivateThisCaseClass(1)
    val y = x.pickle.unpickle[NestedPrivateThisCaseClass]
    assert(x.toString == y.toString)
  }

  test("extendedCaseClass") {
    implicit val pu = {
      // TODO - We use runtime generation here because we don't have a sufficient algorithm to handle final/serializable but NON-case class classes.
      implicit val nested: Pickler[OpenCaseClassSub] with Unpickler[OpenCaseClassSub] = {
        val cls = classOf[OpenCaseClassSub]
        import scala.pickling.internal.currentRuntime
        val key = FastTypeTag[OpenCaseClassSub]
        PicklerUnpickler(
          currentRuntime.picklers.genPickler(cls.getClassLoader, cls, key).asInstanceOf[Pickler[OpenCaseClassSub]],
          currentRuntime.picklers.genUnpickler(currentRuntime.currentMirror, key.key).asInstanceOf[Unpickler[OpenCaseClassSub]]
        )
      }
      PicklingMacros.genPicklerUnpickler[OpenCaseClass]
    }
    val x = OpenCaseClass(1)
    val y = x.pickle.unpickle[OpenCaseClass]
    assert(x == y)
    val x1: OpenCaseClass = new OpenCaseClassSub(1, 2)
    val y1 = x1.pickle.unpickle[OpenCaseClass]
    assert(x1 == y1)
  }

  test("ignoreSubclasses") {
    import generator.opts.ignoreCaseClassSubclasses
    import static._
    case class Other(x: Int)
    implicit val pu = {
      PicklingMacros.genPicklerUnpickler[OpenCaseClass]
    }
    implicit val pu2 = PicklingMacros.genPicklerUnpickler[Other]
    val x = OpenCaseClass(1)
    // Because we ignore subclasses, we shouldn't freak about subclass tags, because we ignore them. Only the
    // "shape" of the pickle should matter.
    val y = x.pickle.unpickle[Other]
    assert(x.x == y.x)
  }
}

// Case 1 - empty
final case class CaseClassNoConstructor()
// Case 2 - simple
final case class SimpleCaseClass(x: Int, y: String)
// Case 3 - varags
final case class MultipleParamListCaseClass(x: Int)(val y: String)
// Case 4 - Nested public var
final case class NestedVarCaseClass(x: Int) { var y: Int = 0 }
// Case 5 - Embedded in class
final class NestedCaseClassHolder {
  final case class NestedCaseClass(x: Int)
  object NestedCaseClass {
    import scala.pickling.Defaults._
    implicit val p = PicklingMacros.genPicklerUnpickler[NestedCaseClass]
  }
}
// Case 6 - Nested public val
case class NestedValCaseClass(x: Int) {
  val y = "Hi"
}
// Case 7 - protected val
case class ProtectedMemberCaseClass(x: Int, protected val y: String)
// Case 8 - private val
case class PrivateMemberCaseClass(x: Int, private val y: String)

// Case 9 - private cosntructor (SCALA BUG PREVENTS THIS FROM COMPILING)
final case class PrivateConstructorCaseClass private (x: Int, y: String) {}

// Case 10 - nested private var
final case class NestedPrivateVarCaseClass(x: Int) {
  private var y = NestedPrivateVarCaseClass.globalY
  override def equals(x: Any): Boolean =
    x match {
      case null => false
      case x: NestedPrivateVarCaseClass => (x.y == y) && (x.x == x.x)
      case _ => false
    }
  override def toString = s"NestedPrivateVarCaseClass($x) { var y = $y }"
}
object NestedPrivateVarCaseClass {
  private var globalY = 1
  private def nextY = {
    globalY += 1
    globalY
  }
}

// Case 11 - nested private[this]
final case class NestedPrivateThisCaseClass(x: Int) {
  private[this] val y = NestedPrivateThisCaseClass.globalY
  override def toString = s"NestedPrivateThisCaseClass($x) { var y = $y }"
}
object NestedPrivateThisCaseClass {
  private var globalY = 1
  private def nextY = {
    globalY += 1
    globalY
  }
}

@directSubclasses(Array(classOf[OpenCaseClassSub]))
case class OpenCaseClass(x: Int)
final class OpenCaseClassSub(x: Int, var y: Int) extends OpenCaseClass(x) {
  override def equals(other: Any): Boolean =
    other match {
      case value: OpenCaseClassSub => (value.y == y) && (value.x == x)
      case _ => false
    }
  override def toString = s"OpenCaseClassSub($x, $y)"
}