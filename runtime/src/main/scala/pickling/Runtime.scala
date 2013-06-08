package scala.pickling

import scala.reflect.runtime.universe._
import definitions._
import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.ToolBox
import ir._

class CompiledPicklerRuntime(classLoader: ClassLoader, clazz: Class[_]) extends PicklerRuntime(classLoader, clazz) {
  override def genPickler(implicit format: PickleFormat): SPickler[_] = {
    // TODO: we should somehow cache toolboxes. maybe even inside the reflection API
    // TODO: toolbox bug. if we don't explicitly import PickleOps, it will fail to be found
    // more precisely: it will be found, but then immediately discarded, because a reference to it won't typecheck
    val formatTpe = mirror.reflect(format).symbol.asType.toType
    mirror.mkToolBox().eval(q"""
      import scala.pickling._
      import scala.pickling.`package`.PickleOps
      implicit val format: $formatTpe = new $formatTpe()
      implicitly[SPickler[$tpe]]
    """).asInstanceOf[SPickler[_]]
  }
}

// TODO: copy/paste wrt CompiledPicklerRuntime
class CompiledUnpicklerRuntime(mirror: Mirror, tag: TypeTag[_]) {
  def genUnpickler(implicit format: PickleFormat): Unpickler[_] = {
    // see notes and todos in CompiledPicklerRuntime.genPickler
    val formatTpe = mirror.reflect(format).symbol.asType.toType
    mirror.mkToolBox().eval(q"""
      import scala.pickling._
      import scala.pickling.`package`.PickleOps
      implicit val format: $formatTpe = new $formatTpe()
      implicitly[Unpickler[$tag]]
    """).asInstanceOf[Unpickler[_]]
  }
}
