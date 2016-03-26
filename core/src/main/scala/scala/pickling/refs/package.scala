package scala.pickling

package object shareEverything {
  @deprecated("Sharing is not guaranteed to be safe w/ all possible picklers.")
  implicit object ShareEverything extends refs.ShareEverything
}

package object shareNothing {
  @deprecated("Sharing is not guaranteed to be safe w/ all possible picklers.")
  implicit object ShareNothing extends refs.ShareNothing
}

package refs {
  @deprecated("Sharing is not guaranteed to be safe w/ all possible picklers.")
  final class Ref

  sealed trait Share
  object Share {
    @deprecated("Sharing is not guaranteed to be safe w/ all possible picklers.")
    implicit object ShareNonPrimitives extends ShareNonPrimitives
  }

  sealed trait ShareNonPrimitives extends Share
  sealed trait ShareEverything extends Share
  sealed trait ShareNothing extends Share
}