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
      x.dispatchTo.pickle(x)
      //format.write(ir)
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

    val pickleFormatClazz = m.staticClass(pickleFormatTree.tpe.toString)

    val cm = m.reflectClass(pickleFormatClazz)
    val ctor = pickleFormatClazz.toType.declaration(ru.nme.CONSTRUCTOR).asMethod
    val ctorm = cm.reflectConstructor(ctor)

    val pickleFormat = ctorm().asInstanceOf[PickleFormat]

    // println("clazz: "+pickleFormatClazz)
    // println("PickleFormat instance: "+pickleFormat)

    val tt = weakTypeTag[T]
    val fields = tt.tpe.declarations.filter(!_.isMethod)

    // build IR
    val irs = new IRs[c.type](c)
    val ir = irs.ObjectIR(tt.tpe, fields.map(field => irs.FieldIR(field.name.toString, field.typeSignatureIn(tt.tpe))).toList)

    val pickleFormatExpr = c.Expr[pickleFormat.type](pickleFormatTree)

    // reify(null)

    reify {
      new Pickler[T] {
        def pickle(obj: Any): Pickle = {
          val pf = pickleFormatExpr.splice
          val fldTempls = fields.map(field => pf.genFieldTemplate(c)(field.name.toString))
          val objTempl = pf.genObjectTemplate(c)(tt.tpe, fldTempls.toList)
          // pf.build(objTempl(List("Bob", 42)))
          null
        }
      }
    }
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
    def pickle(obj: Any): Pickle
    //def unpickle(p: Pickle): T
  }

  trait Pickle {
    val value: Any
  }

  trait HasPicklerDispatch {
    def dispatchTo: Pickler[_]
  }

  // PickleFormat is intended to be used at compile time
  // to generate a pickle template
  trait PickleFormat {
    type Data
    def genObjectTemplate(c: Context)(tpe: c.universe.Type, fields: List[c.Expr[Any => Data]]): c.Expr[List[Any] => Data]
    def genFieldTemplate(c: Context)(name: String): c.Expr[Any => Data]
    def build(d: Data): Pickle

    // called at compile time to obtained pickled type
    // def pickleType(c: Context)(tpe: c.universe.Type): Any
    // def genPickleTemplate(c: Context)(ir: IR):
  }
}


