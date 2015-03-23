package scala.pickling
package ir

import scala.reflect.api.Universe


import HasCompat._
import scala.reflect.internal.ClassfileConstants
import scala.reflect.io.AbstractFile
import scala.tools.asm._
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


class JavaIRs[U <: Universe with Singleton](val uni: U, log: JavaIRLogger) {
  import uni._
  import compat._
  import definitions._


  case class JavaFieldInfo(name: String, tpe: Type)

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
      private var tpe: Option[Type] = None
      private var mkType: Type => Type = identity

      override def visitFormalTypeParameter(name: String): Unit = ???
      override def visitClassBound(): SignatureVisitor = ???
      override def visitInterfaceBound(): SignatureVisitor = ???
      // TODO - What does this mean, and why do we always see it.
      override def visitSuperclass(): SignatureVisitor = this
      override def visitInterface(): SignatureVisitor = ???
      override def visitParameterType(): SignatureVisitor = ???
      override def visitReturnType(): SignatureVisitor = ???
      override def visitExceptionType(): SignatureVisitor = ???
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
        tpe = Some(mkType(myTpe))
        mkType = identity
      }
      override def visitTypeVariable(name: String): Unit = ???
      override def visitArrayType(): SignatureVisitor = {
        mkType = { tpe: Type =>
          uni.appliedType(typeOf[Array[_]].typeConstructor, tpe :: Nil)
        }
        this
      }
      override def visitClassType(name: String): Unit = {
        // TODO - is this correct, it feels wrong...?
        tpe = Some(mkType(uni.rootMirror.staticClass(name.replaceAllLiterally("/", ".")).asType.toTypeConstructor))
        mkType = identity
      }
      override def visitInnerClassType(name: String): Unit = ???
      override def visitTypeArgument(): Unit = ???
      override def visitTypeArgument(wildcard: Char): SignatureVisitor = ???
      override def visitEnd(): Unit = ()
      final def toType: Type = {
        tpe getOrElse log.abort(s"Could not reify java signature into scala type, sig: $sig")
      }
    }

  }

  // TODO - Determine an API to grab fields that are not transient.
  class FieldMarkerVisitor(tpe: Type) extends ClassVisitor(Opcodes.ASM5) {

    private var fields: Seq[JavaFieldInfo] = Nil

    def getFields: Seq[JavaFieldInfo] = fields

    // TODO - Check not abstract!
    // Called when visiting a class.  We use this to check inheritence.
    override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
      if((access & Opcodes.ACC_ABSTRACT) > 0) {
        log.error(s"Unable to generate (un)pickler for abstract Java type: $tpe")
      }

      if((access & Opcodes.ACC_FINAL) == 0) {
        log.warn(s"Warning: Generating a pickler for a java type which is not final/closed: $tpe\nSubclasses of $tpe will not be deserialized.")
      }

      if(superName != "java/lang/Object") {
        // TODO - We'll need to go visit the super class for fields
        log.error(s"Super-class support not implemented.  Found $name extends $superName")
      }
      // TODO - are all interfaces listed?
      if(!(interfaces contains "java/io/Serializable")) {
        log.error(s"Cannot serialize a non-serializable class, $name.  Found interfaces: ${interfaces.mkString(",")}")
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
        val fieldTpe = JavaSignatureToScalaType.reify(desc)
        // TODO - We need to find a way to test if a Java type requires a type parameter and we do not have it.
        fields = JavaFieldInfo(name, fieldTpe) +: fields
      }
      object printFieldVisitor extends FieldVisitor(Opcodes.ASM5) {
        override def visitTypeAnnotation(typeRef: Int, typePath: TypePath , desc: String , visible: Boolean): AnnotationVisitor = {
          System.err.println(s"visitTypeAnnotation($typeRef, $typePath, $desc, $visible)")
          null
        }
        override def visitAttribute(attr: Attribute ): Unit = {
          System.err.println(s"visitAttribute($attr)")
        }
      }
      printFieldVisitor
    }
  }
  def newClassIR(tpe: Type) = {
    val g = evilGlobal
    val cls = g.exitingFlatten(tpe.asInstanceOf[g.Type].typeSymbol.javaClassName).toString
    log.info(s"Looking for Java class $cls")
    readClass(cls) match {
      case Some(cr) =>
        log.info(s"Visiting class $cls")
        val fields = {
          val visitor = new FieldMarkerVisitor(tpe)
          cr.accept(visitor, 0 /* TODO - flags */)
          visitor.getFields
        }
        fields foreach System.err.println
        ???
      case None =>
        log.abort(s"Could not load classfile for type: $tpe")
    }
  }
}
