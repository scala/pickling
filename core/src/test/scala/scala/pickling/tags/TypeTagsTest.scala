package scala.pickling.tags

import org.scalatest.FunSuite
import scala.reflect.runtime.{universe => ru, currentMirror}
import scala.language.existentials

case class SimpleCaseClass(x: String)
object Nested {
  case class NestedCaseClass(x: String)
}
class Nested {
  case class ClassNestedCaseClass(x: String)
}
class TagTest extends FunSuite {
 
  test("makeRaw is consistent") {
    val tags = List(
      // Simple types.
      FastTypeTag[Int],
      FastTypeTag[String],
      FastTypeTag[SimpleCaseClass],
      
      // Singleton types
      FastTypeTag[SimpleCaseClass.type],
      FastTypeTag[List.type],
      
      // Existentials
      FastTypeTag[java.util.List[_]],
      FastTypeTag[Option[_]],
      FastTypeTag[Some[_]],
      FastTypeTag[List[_]],
      FastTypeTag[List[T] forSome { type T <: Any }],
      // Broken...
      //FastTypeTag[x.ClassNestedCaseClass forSome { val x: Nested }],
      
      // Compund types
      FastTypeTag[String with Serializable],
      FastTypeTag[Any => Any],
      
      // Projections
      FastTypeTag[Nested.type#NestedCaseClass],
      // TODO - This fails, but is also something we couldn't pickle statically.
      // Indeed it's actually impossible to even reflect the physical class with an instance of the outer class.
      //FastTypeTag[Nested#ClassNestedCaseClass], // scala.pickling.tags.Nested$ClassNestedCaseClass  

      // Annotated Types
      FastTypeTag[Int @annotation.unchecked.uncheckedVariance]
    )
    def test[T](tag: FastTypeTag[T]): Unit = {
      val tpe = tag.reflectType(currentMirror)
      val cls = 
        if (tpe.typeSymbol.isClass || tpe.typeSymbol.isModuleClass) currentMirror.runtimeClass(tpe.typeSymbol.asClass)
        else sys.error(s"Odd error: $tpe is not a class!")
      val clsTag = FastTypeTag.makeRaw(cls)
      assert(tag == clsTag)
    }
    tags foreach (t => test(t))
  }
}
