package scala.reflect
package api

private[scala] trait StdCreators {
  self: Universe =>

  private[scala] case class FixedMirrorTreeCreator(mirror: scala.reflect.api.Mirror[StdCreators.this.type], tree: Tree) extends TreeCreator {
    def apply[U <: Universe with Singleton](m: scala.reflect.api.Mirror[U]): U # Tree =
      if (m eq mirror) tree.asInstanceOf[U # Tree]
      else throw new IllegalArgumentException(s"Expr defined in $mirror cannot be migrated to other mirrors.")
  }

  private[scala] case class FixedMirrorTypeCreator(mirror: scala.reflect.api.Mirror[StdCreators.this.type], tpe: Type) extends TypeCreator {
    def apply[U <: Universe with Singleton](m: scala.reflect.api.Mirror[U]): U # Type =
      if (m eq mirror) tpe.asInstanceOf[U # Type]
      else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be migrated to other mirrors.")
  }
}