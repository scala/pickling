package scala.pickling

import scala.reflect.api.Universe
import scala.reflect.macros.Context

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

abstract class Macro extends scala.reflect.macros.Macro {
  import c.universe._
  import definitions._

  val tools = new Tools[c.universe.type](c.universe)
  val irs = new ir.PickleIRs[c.universe.type](c.universe)

  def instantiatePickleFormat(pickleFormat: Tree): PickleFormat = {
    def failPickleFormat(msg: String) = c.abort(c.enclosingPosition, s"$msg for ${pickleFormat} of type ${pickleFormat.tpe}")
    val pickleFormatCarrier = c.typeCheck(q"$pickleFormat.instantiate", silent = true)
    pickleFormatCarrier.attachments.all.find(_.isInstanceOf[PickleFormat]) match {
      case Some(pf: PickleFormat) => pf
      case _ => failPickleFormat("Couldn't instantiate PickleFormat")
    }
  }

  def pickleType(pickleFormat: Tree): Type = {
    def failPickleFormat(msg: String) = c.abort(c.enclosingPosition, s"$msg for ${pickleFormat} of type ${pickleFormat.tpe}")
    val pickleTypeCarrier = c.typeCheck(tq"${pickleFormat.tpe}#PickleType", mode = c.TYPEmode, silent = true)
    pickleTypeCarrier match {
      case EmptyTree => failPickleFormat("Couldn't resolve PickleType")
      case tree => tree.tpe.normalize match {
        case tpe if tpe.typeSymbol.isClass => tpe
        case tpe => failPickleFormat(s"PickleType resolved as $tpe is invalid")
      }
    }
  }

  def pickleFormatType(pickleTree: Tree): Type = {
    def failUnpickle(msg: String) = c.abort(c.enclosingPosition, s"$msg for $pickleTree of type ${pickleTree.tpe}")
    val pickleFormatTypeCarrier = c.typeCheck(tq"${pickleTree.tpe}#PickleFormatType", mode = c.TYPEmode, silent = true)
    pickleFormatTypeCarrier match {
      case EmptyTree => failUnpickle("Couldn't resolve PickleFormatType")
      case tree => tree.tpe.normalize match {
        case tpe if tpe.typeSymbol.isClass => tpe
        case tpe => failUnpickle(s"PickleFormatType resolved as $tpe is invalid")
      }
    }
  }

  def allStaticallyKnownSubclasses(tpe: Type): List[Type] = {
    tools.allStaticallyKnownSubclasses(tpe.typeSymbol, rootMirror).map(_.asType.toType)
  }
}