/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.language.experimental.macros

// need several things:
// - implicit class
// - pickler trait
// - pickle format trait
// - ir classes, could be in a subpackage, pickling.ir
package object pickling {

  import ir._

  implicit class PickleOps[T <% HasPicklerDispatch](x: T) {
    def pickle(implicit format: PickleFormat): Pickle = {
      val ir = x.dispatchTo.pickle(x)
      format.write(ir)
    }
  }

  implicit def genPickler[T]: Pickler[T] = macro genPicklerImpl[T]

  import scala.reflect.macros.Context
  def genPicklerImpl[T: c.WeakTypeTag](c: Context): c.Expr[Pickler[T]] = {
    import c.universe._

    val pickleFormatTree: Tree = c.inferImplicitValue(typeOf[PickleFormat]) match {
      case EmptyTree => c.abort(c.enclosingPosition, "Couldn't find implicit PickleFormat")
      case tree => tree
    }
    println("type of PickleFormat: "+pickleFormatTree.tpe)

    // now, let's create an instance of that class for use right here, at compile time
    // couldn't get c.eval to work here, AssertionError.
    // using "runtime" reflection in the compiler might not be the best way to achieve this.
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val cm = m.reflectClass(pickleFormatClazz)
    val ctor = pickleFormatClazz.toType.declaration(ru.nme.CONSTRUCTOR).asMethod
    val ctorm = cm.reflectConstructor(ctor)

    val pickleFormatClazz = m.staticClass(pickleFormatTree.tpe.toString)
    val pickleFormat = ctorm()

    // println("clazz: "+pickleFormatClazz)
    // println("PickleFormat instance: "+pickleFormat)


    /*def buildIR(v: toBePickled[c.Expr[Any]]): c.Expr[IR] = {
      null
    }*/

    val tt = weakTypeTag[T]
    val fields = tt.tpe.declarations.filter(!_.isMethod)

    // build IR
    val irs = new IRs(c.universe)
    val ir = irs.ObjectIR(tt.tpe, fields.map(field => irs.FieldIR(field.name, field.typeSignatureIn(tt.tpe))))
/*
    def pickleCode: c.Expr[Unit] = fields foreach {
      case field =>
        field.typeSignature
        reify { newField() }
    }
*/
    reify(null)
/*
    reify { // creates c.Expr[Pickler[T]]
      new Pickler[T] {
        def pickle(x: T): Pickle =
          //newObject(tpe, fields.map(fld => newField(fld.name, mkJSONString(fld.value))

          // buildPickle(List(x.name, x.age)): Pickle




          // val fldIRs = List(FieldIR("name", ???, fld1), FieldIR("age", ???, /))
          // ObjectIR(typeOf[T], fldIRs)
          null
          //ObjectIR(cool.splice/* instance of ru.Type*/, )
        //def unpickle ...
      }
    }*/
  }
}

package pickling {
  import ir._
  import scala.reflect.macros.Context

  trait Pickler[T] {
    def pickle(obj: Any): IR
    def unpickle(p: Pickle): T
  }

  trait Pickle {
    type Data
    val value: Data
  }

  trait HasPicklerDispatch {
    def dispatchTo: Pickler[_]
  }

  trait PickleFormat {
    def newField(name: String, value: Any): String
    def newObject(tpe: Any, fields: List[String]): String
    // called at compile time to obtained pickled type
    def pickleType(c: Context)(tpe: c.universe.Type): Any

    def write(ir: IR): Pickle
    def read(p: Pickle): IR
  }
}


