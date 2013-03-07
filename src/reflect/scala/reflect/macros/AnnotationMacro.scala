package scala.reflect
package macros

trait AnnotationMacro extends Macro {
  override val c: AnnotationContext
}