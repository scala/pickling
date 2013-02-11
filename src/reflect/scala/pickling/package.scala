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

  import scala.reflect.macros.Context
  import ir._

  // TOGGLE DEBUGGING
  var debugEnabled: Boolean = true
  def debug(output: => String) = if (debugEnabled) println(output)

  implicit class PickleOps[T](x: T) {
    def pickle(implicit pickler: Pickler[T], format: PickleFormat): Pickle = {
      pickler.pickle(x)
      //format.write(ir)
    }
  }

  implicit def genPickler[T]: Pickler[T] = macro genPicklerImpl[T]

  def genPicklerExpr[T: c.WeakTypeTag](c: Context)(tpe: c.Type): c.Expr[Pickler[T]] = {
    import c.universe._

    val pickleFormatTree: Tree = c.inferImplicitValue(typeOf[PickleFormat]) match {
      case EmptyTree => c.abort(c.enclosingPosition, "Couldn't find implicit PickleFormat")
      case tree => tree
    }

    // get instance of PickleFormat
    val pickleFormat = c.eval(c.Expr[PickleFormat](c.resetAllAttrs(pickleFormatTree)))

    // get all declared fields (and not accessor methods)
    val fields = tpe.declarations.filter(!_.isMethod)

    // this is unneeded now, but it's useful for debugging
    //--from here
    val implicitPicklers = fields.map{ field =>
      c.inferImplicitValue(
        typeRef(NoPrefix, typeOf[Pickler[_]].typeSymbol, List(field.typeSignatureIn(tpe)))
      )
    }
    debug("Implicit values found per field: " + implicitPicklers)
    //--to here

    var fieldIR2Pickler: Map[FieldIR, c.Tree] = Map()

    // build IR
    val pickledType = pickleFormat.genTypeTemplate(c)(tpe)
    val ir = ObjectIR(pickledType, (fields.map { field =>
      val pickledFieldType = pickleFormat.genTypeTemplate(c)(field.typeSignatureIn(tpe))
      val fir = FieldIR(field.name.toString.trim, pickledFieldType)

      // infer implicit pickler, if not found, try to generate pickler for field
      c.inferImplicitValue(
        typeRef(NoPrefix, typeOf[Pickler[_]].typeSymbol, List(field.typeSignatureIn(tpe)))
      ) match {
        case EmptyTree =>
          debug("I have to look for a pickler for one of my fields: " + field.typeSignatureIn(tpe))
          val fieldPickler = genPicklerExpr[Any](c)(field.typeSignatureIn(tpe))
          debug("I generated a pickler for that field: " + fieldPickler)
          fieldIR2Pickler += (fir -> fieldPickler.tree)
        case tree =>
          fieldIR2Pickler += (fir -> tree)
      }

      fir
    }).toList)

    val chunked: (List[Any], List[FieldIR]) = pickleFormat.genObjectTemplate(ir)
    val chunks = chunked._1
    val holes  = chunked._2
    debug("chunks: "+chunks.mkString("]["))
    debug("chunks.size:" + chunks.size)
    debug("holes.size:" + holes.size)

    // fill in holes
    def genFieldAccess(ir: FieldIR): c.Tree = {
      // obj.fieldName
      debug("selecting member [" + ir.name + "]")
      fieldIR2Pickler.get(ir) match {
        case None =>
          Select(Select(Select(Ident("obj"), ir.name), "pickle"), "value")
        case Some(picklerTree) =>
          Select(Apply(Select(picklerTree, "pickle"), List(Select(Ident("obj"), ir.name))), "value")
          //Select(Ident("obj"), ir.name)
      }
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

    val castAndAssignTree: c.Tree =
      ValDef(Modifiers(), "obj", TypeTree(tpe),
        TypeApply(Select(Ident("raw"), "asInstanceOf"), List(TypeTree(tpe)))
      )

    // pass the assembled pickle into the generated runtime code
    reify {
      new Pickler[T] {
        def pickle(raw: Any): Pickle = {
          //val obj = raw.asInstanceOf[T]
          c.Expr[Unit](castAndAssignTree).splice
          new Pickle {
            val value = {
              c.Expr[Any](concatAllTree).splice
            }
          }
        }
      }
    }
  }

  def genPicklerImpl[T: c.WeakTypeTag](c: Context): c.Expr[Pickler[T]] = {
    import c.universe._
    val tt = weakTypeTag[T]
    genPicklerExpr[T](c)(tt.tpe)
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


