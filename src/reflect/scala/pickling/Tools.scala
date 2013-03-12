package scala.pickling

import scala.reflect.api.Universe

class Tools[U <: Universe with Singleton](val u: U) {
  import u._
  import definitions._
  def allStaticallyKnownSubclasses(sym: Symbol, mirror: Mirror): List[Symbol] = {
    if (sym == AnyClass) Nil // TODO: what other base classes do we want to ignore?
    else {
      // TODO: doesn't work. find out why
      // def loop(sym: Symbol, in: Symbol): List[Symbol] = {
      //   val inType = in.typeSignature
      //   val subclasses  = inType.members.filter { m =>
      //     (m.isClass || m.isModule) &&
      //     m.asType.typeSignature.baseClasses.contains(sym)
      //   }.toList
      //   val subpackages = inType.members.filter(_.isPackage).toList
      //   subclasses ++ subpackages.flatMap(pkg => loop(sym, pkg))
      // }
      // loop(sym, mirror.RootClass)
      List(sym)
    }
  }
}