package scala.reflect.macros
package runtime

import scala.reflect.internal.Flags._
import scala.reflect.runtime.ReflectionUtils

trait JavaReflectionRuntimes {
  self: scala.tools.nsc.typechecker.Analyzer =>

  trait JavaReflectionResolvers {
    self: MacroRuntimeResolver =>

    import global._

    def resolveJavaReflectionRuntime(classLoader: ClassLoader): MacroRuntime = {
      macroLogVerbose(s"[java reflection resolver] resolved implementation as $className.$methName")

      try {
        macroTraceVerbose("classloader is: ")(ReflectionUtils.show(classLoader))
        val implClass = Class.forName(className, true, classLoader)
        val implMeths = implClass.getDeclaredMethods.find(_.getName == methName)
        // relies on the fact that macro impls cannot be overloaded
        // so every methName can resolve to at maximum one method
        val implMeth = implMeths getOrElse { throw new NoSuchMethodException(s"$className.$methName") }
        macroLogVerbose("successfully loaded macro impl as (%s, %s)".format(implClass, implMeth))
        args => {
          val implObj =
            if (isBundle) implClass.getConstructor(classOf[scala.reflect.macros.Context]).newInstance(args.c)
            else implClass.getField("MODULE$").get(null)
          val implArgs = if (isBundle) args.others else args.c +: args.others
          implMeth.invoke(implObj, implArgs.asInstanceOf[Seq[AnyRef]]: _*)
        }
      } catch {
        case ex: Exception =>
          macroTraceVerbose(s"macro runtime failed to load: ")(ex.toString)
          if (flavor == FLAVOR_EXPAND) macroDef setFlag IS_ERROR
          null
      }
    }
  }
}