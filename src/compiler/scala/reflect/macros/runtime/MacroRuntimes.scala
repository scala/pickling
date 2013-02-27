package scala.reflect.macros
package runtime

import scala.collection.mutable.{Map => MutableMap}
import scala.tools.nsc.util.ScalaClassLoader

trait MacroRuntimes extends JavaReflectionRuntimes with ScalaReflectionRuntimes with JitRuntimes {
  self: scala.tools.nsc.typechecker.Analyzer =>

  import global._

  /** Produces a function that can be used to invoke macro implementation for a given macro definition:
   *    1) Looks up macro implementation symbol in this universe.
   *    2) Loads its enclosing class from the macro classloader.
   *    3) Loads the companion of that enclosing class from the macro classloader.
   *    4) Resolves macro implementation within the loaded companion.
   *
   *  @return Requested runtime if macro implementation can be loaded successfully from either of the mirrors,
   *          `null` otherwise.
   */
  private val macroRuntimesCache = perRunCaches.newWeakMap[Symbol, MutableMap[MacroRuntimeFlavor, MacroRuntime]]
  def macroRuntime(macroDef: Symbol, flavor: MacroRuntimeFlavor): MacroRuntime = {
    macroTraceVerbose(s"looking for ${flavorNames(flavor)} macro runtime: ")(macroDef)
    if (fastTrack contains macroDef) {
      if (flavor == FLAVOR_EXPAND) {
        macroLogVerbose("macro expansion is serviced by a fast track")
        fastTrack(macroDef)
      } else {
        macroLogVerbose(s"${flavorNames(flavor)} for a fast track macro => ignoring")
        null
      }
    } else {
      val symbolRuntimesCache = macroRuntimesCache.getOrElseUpdate(macroDef, MutableMap())
      symbolRuntimesCache.getOrElseUpdate(flavor, new MacroRuntimeResolver(macroDef, flavor).resolveRuntime())
    }
  }

  /** Macro classloader that is used to resolve and run macro implementations.
   *  Loads classes from from -cp (aka the library classpath).
   *  Is also capable of detecting REPL and reusing its classloader.
   *
   *  When -Xmacro-jit is enabled, we sometimes fallback to on-the-fly compilation of macro implementations,
   *  which compiles implementations into a virtual directory (very much like REPL does) and then conjures
   *  a classloader mapped to that virtual directory.
   */
  lazy val defaultMacroClassloader: ClassLoader = {
    val classpath = global.classPath.asURLs
    macroLogVerbose("macro classloader: initializing from -cp: %s".format(classpath))
    val loader = ScalaClassLoader.fromURLs(classpath, self.getClass.getClassLoader)

    // a heuristic to detect the REPL
    if (global.settings.exposeEmptyPackage.value) {
      macroLogVerbose("macro classloader: initializing from a REPL classloader: %s".format(global.classPath.asURLs))
      import scala.tools.nsc.interpreter._
      val virtualDirectory = global.settings.outputDirs.getSingleOutput.get
      new AbstractFileClassLoader(virtualDirectory, loader) {}
    } else {
      loader
    }
  }

  /** Flavors of macro runtime, varying based on what the compiler wants from a macro.
   */
  type MacroRuntimeFlavor = Int
  final val FLAVOR_EXPAND: MacroRuntimeFlavor = 1
  final val FLAVOR_ONINFER: MacroRuntimeFlavor = 2
  val flavorNames = Map(FLAVOR_EXPAND -> "expand", FLAVOR_ONINFER -> "onInfer")

  /** Abstracts away resolution of macro runtimes.
   */
  type MacroRuntime = MacroArgs => Any
  class MacroRuntimeResolver(val macroDef: Symbol, val flavor: MacroRuntimeFlavor) extends JavaReflectionResolvers
                                                                                      with ScalaReflectionResolvers
                                                                                      with JitResolvers {
    val binding = loadMacroImplBinding(macroDef)
    val isBundle = binding.isBundle
    val className = binding.className
    val methName =
      if (flavor == FLAVOR_EXPAND) binding.methName
      else if (flavor == FLAVOR_ONINFER) "onInfer"
      else abort(s"${flavorNames(flavor)} $macroDef")
    val methSymbol = attachedMacroImpl(macroDef) map (macroImpl =>
      if (flavor == FLAVOR_EXPAND) macroImpl
      else if (flavor == FLAVOR_ONINFER) macroImpl.owner.info.declaration(TermName(methName))
      else abort(s"${flavorNames(flavor)} $macroDef")
    )

    def resolveRuntime(): MacroRuntime = {
      if (settings.XmacroJit.value && currentRun.compiles(methSymbol)) {
        resolveJitRuntime()
      } else {
        // TODO: make this work under partest, where thread-unsafety of reflection raises it's ugly head
        // resolveScalaReflectionRuntime(defaultMacroClassloader)
        resolveJavaReflectionRuntime(defaultMacroClassloader)
      }
    }
  }
}