/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.language.experimental.macros

package object pickling {

  implicit class PickleOps[T <% HasPicklerDispatch](x: T) {
    def pickle(implicit format: PickleFormat): Pickle = {
      x.dispatchTo.pickle(x)
      //format.write(ir)
    }
  }

  implicit def genPickler[T]: Pickler[T] = macro genPicklerImpl[T]

  import scala.reflect.macros.Context
  import ir._

  def genPicklerImpl[T: c.WeakTypeTag](c: Context): c.Expr[Pickler[T]] = {
    import c.universe._

    val pickleFormatTree: Tree = c.inferImplicitValue(typeOf[PickleFormat]) match {
      case EmptyTree => c.abort(c.enclosingPosition, "Couldn't find implicit PickleFormat")
      case tree => tree
    }

    // get instance of PickleFormat
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)

    val pickleFormatClazz = m.staticClass(pickleFormatTree.tpe.toString)

    val cm = m.reflectClass(pickleFormatClazz)
    val ctor = pickleFormatClazz.toType.declaration(ru.nme.CONSTRUCTOR).asMethod
    val ctorm = cm.reflectConstructor(ctor)

    val pickleFormat = ctorm().asInstanceOf[PickleFormat]

    val tt = weakTypeTag[T]
    val fields = tt.tpe.declarations.filter(!_.isMethod)
    val implicitPicklers = fields.map{ field =>
      c.inferImplicitValue(
        typeRef(NoPrefix, typeOf[Pickler[_]].typeSymbol, List(field.typeSignatureIn(tt.tpe)))
      )
    }
    println("Implicit values found per field: " + implicitPicklers)

    // build IR
    val pickledType = pickleFormat.genTypeTemplate(c)(tt.tpe)
    val ir = ObjectIR(pickledType, (fields.map { field =>
      val pickledFieldType = pickleFormat.genTypeTemplate(c)(field.typeSignatureIn(tt.tpe))
      FieldIR(field.name.toString.trim, pickledFieldType)
    }).toList)

    val chunked: (List[Any], List[FieldIR]) = pickleFormat.genObjectTemplate(ir)
    val chunks = chunked._1
    val holes  = chunked._2
    println("chunks: "+chunks.mkString("]["))
    println("chunks.size:" + chunks.size)
    println("holes.size:" + holes.size)

    // fill in holes
    def genFieldAccess(ir: FieldIR): c.Tree = {
      // obj.fieldName
      println("selecting member [" + ir.name + "]")
      Select(Ident("obj"), ir.name)
    }

    def genChunkLiteral(chunk: Any): c.Tree =
      Literal(Constant(chunk))

    // assemble the pickle from the template
    val cs = chunks.init.zipWithIndex
    val pickleChunks: List[c.Tree] = for (c <- cs) yield {
      Apply(Select(pickleFormatTree, "concatChunked"), List(genChunkLiteral(c._1), genFieldAccess(holes(c._2))))
    }
    val concatAllTree = (pickleChunks :+ genChunkLiteral(chunks.last)) reduceLeft { (left: c.Tree, right: c.Tree) =>
      Apply(Select(pickleFormatTree, "concatChunked"), List(left, right))
    }

    // pass the assembled pickle into the generated runtime code
    reify {
      new Pickler[T] {
        def pickle(raw: Any): Pickle = {
          val obj = raw.asInstanceOf[T]
          new Pickle {
            val value = {
              c.Expr[Any](concatAllTree).splice
            }
          }
        }
      }
    }
  }
}

package pickling {
  import scala.reflect.macros.Context

  trait Pickler[-T] {
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
  // to generate a pickle template which is to be inlined
  trait PickleFormat {
    import ir._

    type Chunked = (List[Any], List[FieldIR])

    def genTypeTemplate(c: Context)(tpe: c.universe.Type): Any
    def genObjectTemplate(ir: ObjectIR): Chunked
    def genFieldTemplate(ir: FieldIR): Chunked

    def concatChunked(c1: Any, c2: Any): Any
  }
}


