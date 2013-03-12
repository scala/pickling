import scala.annotation.{MacroAnnotation, StaticAnnotation}
import scala.reflect.macros.AnnotationContext
import language.experimental.macros

@java.lang.annotation.Inherited
class fooable extends MacroAnnotation {
  def transform = macro Macros.impl
}

class unfoo extends StaticAnnotation

object Macros {
  def impl(c: AnnotationContext) = {
    import c.universe._
    c.annottee match {
      case ClassDef(Modifiers(flags, privateWithin, annotations), name, tparams, Template(parents, self, body)) =>
        if (annotations.exists(_.toString.contains("unfoo"))) c.annottee
        else {
          val annotations1 = Nil // TODO: find out a good way to exclude the currently expanding annotation
          val foo = DefDef(NoMods, TermName("foo"), Nil, Nil, TypeTree(), Literal(Constant("foo")))
          ClassDef(Modifiers(flags, privateWithin, annotations1), name, tparams, Template(parents, self, body :+ foo))
        }
    }
  }
}