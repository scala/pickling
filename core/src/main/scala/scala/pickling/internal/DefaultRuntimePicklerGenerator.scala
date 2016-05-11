package scala.pickling
package internal

import java.util.concurrent.locks.ReentrantLock
import scala.pickling.runtime.{CustomRuntime, InterpretedUnpicklerRuntime, ShareNothingInterpretedUnpicklerRuntime, RuntimePickler}
import scala.reflect.runtime.universe.Mirror
import scala.reflect.runtime.{universe => ru}

/**
 * Default implementation of a runtime pickler generator
 *
 * TODO - There is still something off here compared to the hardcoded versions.
 *
 *
[error] 	scala.pickling.array.json.ArrayJsonTest
[error] 	scala.pickling.externalizable.mapstatus.MapStatusTest
[error] 	scala.pickling.runtimenulltest.NullRuntimeTest
[error] 	scala.pickling.runtime.RuntimeArrayTests
[error] 	scala.pickling.test.collection.WrappedArrayTest
[error] 	scala.pickling.array.binary.ArrayBinaryTest
[error] 	scala.pickling.test.runtime.array.RuntimeArrayTest
 */
class DefaultRuntimePicklerGenerator(reflectionLock: ReentrantLock)
  extends spi.RuntimePicklerGenerator with CustomRuntime {

  /** Create a new pickler using the given tagKey. */
  def genPickler(tag: FastTypeTag[_])(implicit share: refs.Share): Pickler[_] = {
    System.err.println(s"Generating runtime pickler for $tag, share: $share")
    reflectionLock.lock()
    try {
      val classLoader = currentRuntime.currentClassLoader
      val clazz = tag.reflectClass(classLoader)
      // debug(s"!!! could not find registered pickler for class $className, tag ${tag.key} !!!")
      val pickler: Pickler[_] = if (clazz.isArray) {
        val mirror = currentRuntime.currentMirror
        val elemClass = clazz.getComponentType()
        val elemTag = FastTypeTag.makeRaw(elemClass)
        val elemPickler = currentRuntime.picklers.genPickler(elemTag)
        mkRuntimeTravPickler[Array[AnyRef]](elemClass, elemTag, tag, elemPickler, null)
      } else {
        val runtime = new RuntimePickler(classLoader, clazz, tag)
        runtime.mkPickler
      }
      pickler
    } finally reflectionLock.unlock()
  }

  /** Create a new unpickler using the given tagKey. */
  override def genUnpickler(tag: FastTypeTag[_])(implicit share: refs.Share): Unpickler[_] = {
    reflectionLock.lock()
    try {
      val tagKey = tag.key
      val unpickler = if (tagKey.startsWith("scala.Array")) {
        // debug(s"runtime unpickling of an array: $tagKey")
        val elemTypeString = tagKey.substring(12, tagKey.length - 1)
        // debug(s"creating tag for element type: $elemTypeString")
        // TODO - If the elem tag is not something useful, we should treat it as `Any`...
        val elemTag = FastTypeTag(elemTypeString)
        val elemClass = 
           if (elemTypeString.startsWith("scala.Array")) Classes.classFromString(elemTypeString)
           else Classes.classFromString(elemTag.typeConstructor)
        val elemUnpickler = internal.currentRuntime.picklers.genUnpickler(elemTag)
        val tag = FastTypeTag(tagKey)
        mkRuntimeTravPickler[Array[AnyRef]](elemClass, elemTag, tag, null, elemUnpickler)
      } else {
          val runtime = if (share.isInstanceOf[refs.ShareNothing]) {
            // TODO - These should be using the classloader passed in...
            // debug(s"@@@ creating ShareNothingInterpretedUnpicklerRuntime for type $tagKey")
            new ShareNothingInterpretedUnpicklerRuntime(currentRuntime.currentMirror, tagKey)
          } else {
            // debug(s"@@@ creating InterpretedUnpicklerRuntime for type $tagKey")
            new InterpretedUnpicklerRuntime(currentRuntime.currentMirror, tagKey)
          }
          runtime.genUnpickler

      }
      unpickler
    } finally reflectionLock.unlock()
  }
}
