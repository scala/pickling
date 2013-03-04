package scala.reflect.macros
package contexts

abstract class AnnotationContext extends Context with scala.reflect.macros.AnnotationContext {
  val annottee: Tree
  val companion: Tree

  type Expansion = universe.MacroAnnotationExpansion
  val Expansion = universe.MacroAnnotationExpansion

  def expand(annottee: Tree) = Expansion(List(annottee), None)
  def expand(annottee: Tree, companion: Tree) = Expansion(List(annottee), Some(companion))
  def expand(annottee: List[Tree]) = Expansion(annottee, None)
  def expand(annottee: List[Tree], companion: Tree) = Expansion(annottee, Some(companion))
}
