package scala.pickling
package generator
package scalasymbols

import org.scalatest.FunSuite

case class Foo(x: Int, y: String)
class Baz(x: Int) {
  var y: String = ""
  private[this] var z: Double = 0.0
}

class TransientFoo {
  @transient var y: Int = 0
}
case class TransientFooCase(x: Int, @transient y: Int)


object ParentTag {
  trait Foo[X,Y] {
    var x: (X, Y)
    var y: Y
  }
  trait Baz[Y] extends Foo[Y, Int] {
    var z: (Y, Float)
  }
  trait Biz extends Baz[String] {}

  case class Foo2(x: (Int, String))
}
object ValNames {
  trait Foo {
    val x: Int
    val z: String
  }
}


/**
 * Tests the scala symbol loader for pickling.
 *
 * Basic tests
 * - Annotation detction (transient fields)
 * - Field detection (vars, vals, etc.)
 * - Generic Type handling (e.g.  Bar extends Foo[Int, String] leads to Int and String being remembered in symbols)
 */
class TestScalaSymbols extends FunSuite {


  test("detectNames") {
    assert(Compat.paramNames[Foo] == Seq("x", "y"))
    assert(Compat.paramNames[TransientFooCase] == Seq("x", "y"))
  }

  test("fieldTypesWithTypeArgs") {
    assert(Compat.varTypes[ParentTag.Foo[String, Int]].toSet == Set(
      "(String, Int)", "Int"
    ))
    assert(Compat.varTypes[ParentTag.Baz[String]].toSet == Set(
      "(String, Float)",
      "(String, Int)",
      "Int"
    ))
    assert(Compat.varTypes[ParentTag.Biz].toSet == Set(
      "(String, Float)",
      "(String, Int)",
      "Int"

    ))
  }

  test("constructorParamWithTypeArgs") {
    assert(Compat.constructorParamTypes[ParentTag.Foo2] ==
      Seq("(Int, String)")
    )
  }

  test("parentClassesWithTypeArgs") {
    val parentClassKeys = Compat.parentClassTags[ParentTag.Biz].toSet
    assert(parentClassKeys == Set(
      "scala.pickling.generator.scalasymbols.ParentTag.Foo[String,Int]",
      "scala.pickling.generator.scalasymbols.ParentTag.Baz[String]",
      "Any",
      "Object"))
  }

  test("detectTransient") {
    val transientVars = Compat.getTransientVars[TransientFoo]
    assert(transientVars == Seq("y"))

    val transientVars2 = Compat.getTransientVars[TransientFooCase]
    assert(transientVars2 == Seq("y"))

    // TODO - Note, only the var is detecting as transitive right now.  Which is why this isn't working for
    // will robinson pickling.
    val fields = Compat.fieldNames[TransientFoo]
    assert(fields == Seq("y"))

    val transientFields = Compat.getTransientFieldNames[TransientFoo]
    assert(transientFields == Seq("y"))
  }

  test("detectFieldNames") {
    val fields = Compat.fieldNames[Foo]
    assert(fields.toSet == Set("y", "x"))

    val fields2 = Compat.fieldNames[Baz]
    assert(fields2.toSet == Set("z", "y", "x"))

    val fields3 = Compat.fieldNames[ParentTag.Foo2]
    assert(fields3.toSet == Set("x"))

    val fields4 = Compat.fieldNames[TransientFooCase]
    assert(fields4.toSet == Set("x", "y"))
  }
}
