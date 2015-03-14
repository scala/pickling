package scala.pickling
package ir

import scala.reflect.api.Universe
import java.lang.reflect.Modifier

import HasCompat._
import scala.reflect.io.AbstractFile
import scala.tools.asm.{Opcodes, FieldVisitor, ClassVisitor, ClassReader}

class JavaIRs[U <: Universe with Singleton](val uni: U) {
  import uni._
  import compat._
  import definitions._

  private def evilGlobal: scala.tools.nsc.Global = {
    uni.asInstanceOf[scala.tools.nsc.Global]
  }
  private def classpath = {
    evilGlobal.classPath
  }

  private def findClass(cls: String): Option[AbstractFile] = {
    classpath.findClassFile(cls)
  }

  private def readClass(cls: String): Option[scala.tools.asm.ClassReader] = {
    findClass(cls) match {
      case Some(f) => Some(new ClassReader(f.toByteArray))
      case None => None
    }
  }

  // TODO - Figure out which API to pass to class viistor...
  class FieldMarkerVisitor extends ClassVisitor(Opcodes.ASM5) {
    override def visitField(access: Int, name: String, desc: String, signature: String, value: AnyRef): FieldVisitor  = {
      // TODO - check to see if it's not transient, and record the dude.
      System.err.println(s"Found field: $name")
      null
    }
  }
  def newClassIR(tpe: Type) = {
    val g = evilGlobal
    val cls = g.exitingFlatten(tpe.asInstanceOf[g.Type].typeSymbol.javaClassName).toString
    System.err.println(s"Looking for class $cls")
    readClass(cls) match {
      case Some(cr) =>
        System.err.println(s"Visiting class $cls")
        val visitor = new FieldMarkerVisitor()
        cr.accept(visitor, 0 /* TODO - flags */)
        ???
      case None =>
        sys.error(s"Could not load classfile for type: $tpe")
    }
  }
}
