package scala.reflect
package api

trait Quasiquotes { self: Universe =>

  // implementation is hardwired to methods of `scala.tools.reflect.Quasiquotes`
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  implicit class Quasiquote(ctx: StringContext) {
    object q {
      def apply(args: Any*): Any = macro ???
      def unapply(tree: Any): Option[Any] = macro ???
    }
    object tq {
      def apply(args: Any*): Any = macro ???
      def unapply(tree: Any): Option[Any] = macro ???
    }
  }
}
