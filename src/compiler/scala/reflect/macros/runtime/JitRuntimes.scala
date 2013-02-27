package scala.reflect.macros
package runtime

import scala.reflect.internal.Flags._
import scala.reflect.runtime.ReflectionUtils
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.util.ScalaClassLoader

trait JitRuntimes {
  self: scala.tools.nsc.typechecker.Analyzer =>

  trait JitResolvers {
    self: MacroRuntimeResolver =>

    def resolveJitRuntime(): MacroRuntime = {
      macroLogJit(s"starting JIT compilation of $methSymbol")
      JitCompiler.tryCompile(global)(macroDef, methSymbol) match {
        case Some(vdir) =>
          macroLogJit("JIT compilation has succeeded")
          val parentClassLoader = ScalaClassLoader.fromURLs(global.classPath.asURLs, this.getClass.getClassLoader)
          val jitClassLoader = new AbstractFileClassLoader(vdir, parentClassLoader) {
            override def loadClass(name: String, resolve: Boolean) = {
              findAbstractFile(classNameToPath(name)) match {
                case null => super.loadClass(name, resolve)
                case file =>
                  val bytes = file.toByteArray
                  defineClass(name, bytes, 0, bytes.length)
              }
            }
          }
          resolveJavaReflectionRuntime(jitClassLoader)
        case None =>
          macroLogJit("JIT compilation has failed")
          if (flavor == FLAVOR_EXPAND) macroDef setFlag IS_ERROR
          null
      }
    }
  }
}
