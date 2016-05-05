package scala.pickling
package runtime

import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.{
	runtimeMirror, 
	Mirror
}


// Note: Only works with scala 2.11.8+
object MacroBasedRuntime extends spi.RuntimePicklerGenerator {

  private def genMacroPickler(classLoader: ClassLoader, tag: FastTypeTag[_]): Pickler[_] = {
  	// TODO - Default picklers should be configured
  	// TODO - generate runtime macro which uses similar mechanisms as original, but defers sub-type lookup to
  	//        runtime?
  	val source = s"""
  		import _root_.scala.pickling.pickler.AllPicklers._;
  		_root_.scala.pickling.Pickler.generate[${tag.key}]"""
  	val mirror = runtimeMirror(classLoader)
  	// TODO - we can pass an appropriate classloader to mkToolBox in Scala 2.11.7 and below
  	val toolBox = mirror.mkToolBox()
  	val tree = toolBox.parse(source)
  	System.err.println(tree)
    toolBox.eval(tree).asInstanceOf[Pickler[_]]
  }	
  /** Create a new pickler using the given tagKey. */
  def genPickler(classLoader: ClassLoader, clazz: Class[_], tag: FastTypeTag[_])(implicit share: refs.Share): Pickler[_] = 
    // TODO _ Catch exception and delegate down to pure-java-reflection based pickler.
  	genMacroPickler(classLoader, tag)


  /** Create a new unpickler using the given tagKey. */
  override def genUnpickler(mirror: Mirror, tagKey: String)(implicit share: refs.Share): Unpickler[_] = {
  	???
  }
}