package scala.pickling.tags

import org.scalatest.FunSuite
import scala.reflect.runtime.{universe => ru, currentMirror}

 case class NestedCaseClass(x: String)

class TagTest extends FunSuite {
 
  test("makeRaw is consistent") {
    val tags = List(
      FastTypeTag[List[_]],
      FastTypeTag[Int],
      FastTypeTag[String],
      FastTypeTag[NestedCaseClass],
      FastTypeTag[NestedCaseClass.type],
      FastTypeTag[List.type],
      FastTypeTag[java.util.List[_]],
      FastTypeTag[Option[_]],
      FastTypeTag[Some[_]]
      // TODO - Figure out why these are broken.
      //FastTypeTag[String with Serializable]
      //FastTypeTag[String => Int]
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
