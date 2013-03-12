package scala.reflect
package macros

trait AnnotationContext extends Context {
  def annottee: Tree
  def companion: Tree

  type Expansion
  def expand(expandedAnnottee: Tree): Expansion
  def expand(expandedAnnottee: Tree, expandedCompanion: Tree): Expansion
  def expand(expandedAnnottee: List[Tree]): Expansion
  def expand(expandedAnnottee: List[Tree], expandedCompanion: Tree): Expansion
}
