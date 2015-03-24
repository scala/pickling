package scala.pickling
package ir

import scala.reflect.api.Universe


import HasCompat._
import scala.reflect.internal.ClassfileConstants
import scala.reflect.io.AbstractFile
import scala.tools.asm._
import scala.tools.asm.{Type=>AsmType}
import scala.tools.asm.signature.{SignatureReader, SignatureVisitor, SignatureWriter}


trait JavaIRLogger {
  /** Issues a warning. */
  def warn(msg: String): Unit
  /** Marks compilation as failed and records why. */
  def error(msg: String): Unit
  /** Debugging info for the macro.  Note:  Will not display by default unelss forced. */
  def info(msg: String, force: Boolean = false): Unit
  /** Aborts compilation with the given message. */
  def abort(msg: String): Nothing
}
object JavaIRLogger {
  // A logger intended to be used in a reflective/runtime scenario.
  object empty extends JavaIRLogger {
    /** Issues a warning. */
    override def warn(msg: String): Unit = ()
    /** Marks compilation as failed and records why. */
    override def error(msg: String): Unit = throw new RuntimeException(msg)
    /** Aborts compilation with the given message. */
    override def abort(msg: String): Nothing = throw new RuntimeException(msg)
    /** Debugging info for the macro.  Note:  Will not display by default unelss forced. */
    override def info(msg: String, force: Boolean): Unit = ()
  }
}


class JavaIRs[U <: Universe with Singleton](val uni: U, log: JavaIRLogger) {
  import uni._
  import compat._
  import definitions._


  // Info about a field on ajava class.
  case class JavaFieldInfo(name: String, tpe: Type)
  // Info about a class.
  case class ClassInfo(className: String, fields: Seq[JavaFieldInfo], extendedSerializable: Boolean)

  private val evilGlobal: scala.tools.nsc.Global = {
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
  // Helper to convert JVM bytecode signatures into scala types.
  object JavaSignatureToScalaType {
    /** Converts a java binary signature into a type, if we can. */
    def reify(sig: String): Type = {
      val reader = new JavaSignatureToScalaType(sig)
      new SignatureReader(sig).accept(reader)
      reader.toType
    }



    // TODO - This is not very robust and we basically die on failure.
    private class JavaSignatureToScalaType(sig: String) extends SignatureVisitor(Opcodes.ASM5) {
      // We run this visitor pattern as a state machine of explciit, immutable states.
      private var tpe: Option[Type] = None
      private var state: SignatureReaderState = new BaseState()
      sealed trait SignatureReaderState {
        def handleType(tpe: Type): Unit
        def appendTypeArg(t: Type): Unit
        def finalType: Type
      }
      class BaseState() extends SignatureReaderState {
        var tcons: Type = NoType
        var targs: List[Type] = Nil
        def handleType(theType: Type): Unit = {
          tcons = theType
        }
        def finalType: Type =
          if(targs.isEmpty) tcons
          else uni.appliedType(tcons, targs)

        def appendTypeArg(t: Type): Unit = {
          targs = targs :+ t
        }
      }
      class TypeArgumentState(base: SignatureReaderState) extends SignatureReaderState {
        def handleType(theType: Type): Unit = {
          base.appendTypeArg(theType)
          // Remove ourselves from the state.
          JavaSignatureToScalaType.this.state = base
        }
        // This should not happen....
        def appendTypeArg(t: Type): Unit = ???
        def finalType: Type = base.finalType
      }
      class ArrayState(previous: SignatureReaderState) extends SignatureReaderState {
        def handleType(theType: Type): Unit = {
          // TODO - pop the stack of state?
          previous.handleType(
            uni.appliedType(uni.definitions.ArrayClass, theType)
          )
        }
        def appendTypeArg(t: Type): Unit = previous.appendTypeArg(t)
        def finalType: Type =
          previous.finalType
      }
      // Unhandled scenarios.  Generally not seen in field types we parse.
      override def visitFormalTypeParameter(name: String): Unit = ???
      override def visitClassBound(): SignatureVisitor = ???
      override def visitInterfaceBound(): SignatureVisitor = ???
      override def visitInterface(): SignatureVisitor = ???
      override def visitParameterType(): SignatureVisitor = ???
      override def visitReturnType(): SignatureVisitor = ???
      override def visitExceptionType(): SignatureVisitor = ???
      override def visitInnerClassType(name: String): Unit = ???

      // TODO - What does this mean, and why do we always see it.
      override def visitSuperclass(): SignatureVisitor = this
      override def visitBaseType(descriptor: Char): Unit = {
        // All primitive types
        val myTpe =
          descriptor match {
            case ClassfileConstants.BYTE_TAG => typeOf[Byte]
            case ClassfileConstants.CHAR_TAG => typeOf[Char]
            case ClassfileConstants.DOUBLE_TAG => typeOf[Double]
            case ClassfileConstants.FLOAT_TAG => typeOf[Float]
            case ClassfileConstants.INT_TAG => typeOf[Int]
            case ClassfileConstants.LONG_TAG => typeOf[Long]
            case ClassfileConstants.SHORT_TAG => typeOf[Short]
            case ClassfileConstants.BOOL_TAG => typeOf[Boolean]
          }
        state.handleType(myTpe)
      }
      override def visitTypeVariable(name: String): Unit = {
        // TODO - to correctly handle this we actually need to keep track of the generics of
        //        the java class and reconstitute that type here.
        log.abort(s"Currently, Java generic classes are not supported.\nCannot handle type variable $name, in $sig")
      }
      override def visitArrayType(): SignatureVisitor = {
        state = new ArrayState(state)
        this
      }
      override def visitClassType(name: String): Unit = {
        // TODO - Zaugg sent us this code.
        // It seems to puke on inner classes, as they have a $ in the name.
        if(name contains "$") log.abort(s"Inner class pickling is not handled yet.  Required Pickler for ${name}")
        val baseTpe = uni.internal.thisType(uni.rootMirror.staticClass(name.replaceAllLiterally("/", "."))).widen
        state.handleType(baseTpe)
      }
      // This is called for any "raw" type argument (i.e. wildcard/unbound)
      override def visitTypeArgument(): Unit = {
        // True if we're writing a new type argument...
        // Here, we just add a new skolem/unbound type.
        // TODO - use uni.existentialAbstraction(), according to Adriaan
        state.appendTypeArg(uni.WildcardType)
      }
      // This is called for any bound type argument.
      override def visitTypeArgument(wildcard: Char): SignatureVisitor = {
        // Wildcard willbe one of =, - or +, we think.
        // TODO - Handle the wildcard in the generated type signature!
        wildcard match {
          case '=' => // We're good
          case '+' =>
            // TODO - We may be able to allow this, using "boundedWildcardType"
            log.abort(s"Unable to handle unbound java generics (? extends T).  Found $sig")
          case '-' =>
            log.abort(s"Unable to handle unbound java generics (? super T).  Found $sig")
        }
        state = new TypeArgumentState(state)
        this
      }
      // TODO - How often is this called?
      override def visitEnd(): Unit = {
        tpe = Some(state.finalType)
      }
      final def toType: Type = {
        tpe getOrElse log.abort(s"Could not reify java signature into scala type, sig: $sig")
      }
    }

  }

  // TODO - Determine an API to grab fields that are not transient.
  // TODO - we should be able to parse our parent types...
  class FieldMarkerVisitor(tpe: Type, isParent: Boolean = false) extends ClassVisitor(Opcodes.ASM5) {

    private var fields: Seq[JavaFieldInfo] = Nil
    var wasSerializable: Boolean = false

    def getFields: Seq[JavaFieldInfo] = fields

    // TODO - Check not abstract!
    // Called when visiting a class.  We use this to check inheritence.
    override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
      if(!isParent) {
        if ((access & Opcodes.ACC_ABSTRACT) > 0) {
          log.error(s"Unable to generate (un)pickler for abstract Java type: $tpe")
        }

        if ((access & Opcodes.ACC_FINAL) == 0) {
          log.warn(s"Warning: Generating a pickler for a java type which is not final/closed: $tpe\nSubclasses of $tpe will not be deserialized.")
        }
      }

      if(superName != null && superName != "java/lang/Object") {
        val superInfo = readClassFields(superName.replaceAllLiterally("/", "."), tpe, true)
        if(superInfo.extendedSerializable) wasSerializable = true
        // TODO - Read the parent class.
        // TODO - We'll need to go visit the super class for fields
        // TODO -
        fields = fields ++ superInfo.fields

        //log.error(s"Super-class support not implemented.  Found $name extends $superName")
      }
      // TODO - are all interfaces listed?
      if((interfaces contains "java/io/Serializable")) {
        wasSerializable = true
      }
    }
    // We use these visits to ensure writeObject/readObject methods are not customized. IF they a e
    override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor  = {
      def checkNoMethod(checkName: String, sig: String): Unit = {
        if((name == checkName)) {
          if(desc == sig) {
            log.abort(
              s"""Cannot automatically pickle a Serializable class that has custom readObject/writeObject method.
                 |Please write a custom Pickler/Unpickler for $tpe""".stripMargin)
          }
        }
      }
      // TODO - Do we actually care if writing is customized, or only reading?
      //checkNoMethod("writeObject", "(Ljava/io/ObjectOutputStream;)V")
      checkNoMethod("readObject", "(Ljava/io/ObjectInputStream;)V")
      null
    }

    override def visitField(access: Int, name: String, desc: String, signature: String, value: AnyRef): FieldVisitor  = {
      // TODO - check to see if it's not transient, and record the dude.
      if((Opcodes.ACC_STATIC & access) > 0) {
        log.info(s"Ignoring static field: $name")
      } else if((Opcodes.ACC_TRANSIENT & access) > 0) {
        log.info(s"Ignoring transient field: $name")
      } else {
        val sig =
          signature match {
            case null => desc
            case s => s
          }
        val fieldTpe =
          JavaSignatureToScalaType.reify(sig)
        log.info(s"$name : $fieldTpe has signature $signature")
        // TODO - We need to find a way to test if a Java type requires a type parameter and we do not have it.
        fields = JavaFieldInfo(name, fieldTpe) +: fields
      }
      null
    }
  }

  def readClassFields(className: String, tpe: Type, isParent: Boolean = false): ClassInfo = {
    readClass(className) match {
      case Some(cr) =>
        val visitor = new FieldMarkerVisitor(tpe, isParent)
        cr.accept(visitor, ClassReader.SKIP_CODE)
        // TODO - We should
        ClassInfo(className, visitor.getFields, visitor.wasSerializable)
      case None =>
        log.abort(s"Could not load classfile for type: $tpe")
    }
  }

  def newClassIR(tpe: Type) = {
    val g = evilGlobal
    val cls = g.exitingFlatten(tpe.asInstanceOf[g.Type].typeSymbol.javaClassName).toString
    val info = readClassFields(cls, tpe, isParent = false)
    if(!info.extendedSerializable) log.warn(s"Generating a pickler for a Java class which does not extends Serializable, $tpe")
    info
  }
}
