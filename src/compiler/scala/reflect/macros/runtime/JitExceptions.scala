package scala.reflect.macros
package runtime

case class MacroJitCyclicClosureException(sym: String, forbidden: String)
extends MacroJitException(s"definition of $sym uses $forbidden which is currently being jit-compiled")

case class MacroJitCannotLocateDefTreeException(sym: String)
extends MacroJitException(s"cannot locate the definition tree for $sym")

class MacroJitException(msg: String) extends Exception(msg)
object MacroJitException { def apply(msg: String) = new MacroJitException(msg) }
