package scala.pickling
package runtime

import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.{
	runtimeMirror, 
	Mirror
}


// Note: Only works with scala 2.11.8+
object MacroBasedRuntime extends spi.RuntimePicklerGenerator {

  private def genMacroPicklerUnpickler[T](tag: FastTypeTag[T]): Pickler[T] with Unpickler[T] = {
    // TODO - Default picklers should be configured in some fashion.
    // TODO - generate runtime macro which uses similar mechanisms as original, but defers sub-type lookup to
    //        runtime?
    val source = s"""
      import _root_.scala.pickling.pickler.AllPicklers._;
      _root_.scala.pickling.PicklerUnpickler.generate[${tag.key}]
    """
    // TODO - we can pass an appropriate classloader to mkToolBox in Scala 2.11.7 and below
    val toolBox = internal.currentRuntime.currentMirror.mkToolBox()
    val tree = toolBox.parse(source)
    System.err.println(tree)
    toolBox.eval(tree).asInstanceOf[Pickler[T] with Unpickler[T]]
  }
  /** Create a new pickler using the given tagKey. */
  def genPickler(tag: FastTypeTag[_])(implicit share: refs.Share): Pickler[_] = 
    // TODO _ Catch exception and delegate down to pure-java-reflection based pickler.
  	genMacroPicklerUnpickler(tag)


  /** Create a new unpickler using the given tagKey. */
  override def genUnpickler(tag: FastTypeTag[_])(implicit share: refs.Share): Unpickler[_] =
    // TODO - Catch exception and delgate down to pure-java reflection based picklers.
    genMacroPicklerUnpickler(tag)
}