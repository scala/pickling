package scala.pickling
package ir

import scala.reflect.api.Universe

/** This is the portion of code which can actually generate a pickler/unpickler object instance
  * from the IR AST.
  *
  *
  */
trait SourceGenerator extends Macro with FastTypeTagMacros {
  import c.universe._

  def generatePickleImplFromAst(picklerAst: PicklerAst): c.Tree = {

    def genGetField(x: GetField): c.Tree = {
      def pickleLogic(isStaticallyElided: Boolean, fieldValue: Tree): Tree = {
        val elideHint =
          if(isStaticallyElided) q"b.hintStaticallyElidedType()" else q""
        // TODO - Hint dynamically elided?
        // NOTE; This will look up an IMPLICIT pickler for the value, based on the type.
        //       This is how we chain our macros to find all the valid types.

        q"""
           $elideHint
           _root_.scala.pickling.functions.pickleInto($fieldValue, b)"""
      }

      def putField(getterLogic: Tree, isStaticallyElided: Boolean): c.Tree =
        q"builder.putField(${x.name}, b => ${pickleLogic(isStaticallyElided, getterLogic)})"

      x.getter match {
        case x: IrField =>
        // TODO - handle
          sys.error(s"non-method access not handled currently, trying to obtain field $x from ${x.owner}")
        case y: IrMethod =>
          val staticallyElided = {
            // TODO - Figure this out
            val tpe = y.returnType(c.universe)
            tpe.isEffectivelyFinal || tpe.isEffectivelyPrimitive
          }
          if(y.isPublic) putField(q"picklee.${newTermName(y.methodName)}", staticallyElided)
          else {
            val result = reflectively(newTermName("picklee"), y)(fm => putField(q"${fm}.get.asInstanceOf[${y.returnType(u).asInstanceOf[c.Type]}]", staticallyElided))
            q"""..$result"""
          }
        case _: IrConstructor =>
          // This is a logic erorr
          sys.error(s"Pickling logic error.  Found constructor when trying to pickle field ${x.name}.")
      }
    }
    def genPickleOp(op: PicklerAst): c.Tree =
      op match {
        case PickleBehavior(ops) =>
          q"""..${ops.map(genPickleOp)}"""

        case x: GetField => genGetField(x)
      }
    genPickleOp(picklerAst)
  }

  // TODO - Should we always hint the "oid" of the object?
  def hintShareId: c.Tree = {
    // TODO - Some kind of flag to disable this logic from being generated.
    q"""val oid = _root_.scala.pickling.internal.`package`.lookupPicklee(picklee)
        builder.hintOid(oid)"""
  }



  def generatePicklerClass[T: c.WeakTypeTag](picklerAst: PicklerAst): c.Tree = {
    val tpe = computeType[T]

    // TODO - The pickler logic should actually check to make sure the thing passed is actually a FOO, and
    //        If it is not, we delegate to a runtime pickler.
    //        ALTHOUGH, we could, instead, delegate that as function of the AST that we can encode, so
    //        The presence of runtime code is controlled by generation algorithms, rather than the code generator.
    //        We could still disable the generation here.
    //        This is somethign the current algorithm does which we do not do.
    val picklerLogic =
      q"""
        builder.hintTag(tag)
        builder.beginEntry(picklee)
        $hintShareId
        ${generatePickleImplFromAst(picklerAst)}
        builder.endEntry()
      """
    val picklerName = c.fresh((syntheticBaseName(tpe) + "Pickler"): TermName)
    val createTagTree = super[FastTypeTagMacros].impl[T]
    q"""
      _root_.scala.Predef.locally {
        implicit object $picklerName extends _root_.scala.pickling.Pickler[$tpe] with _root_.scala.pickling.Generated {
          def pickle(picklee: $tpe, builder: _root_.scala.pickling.PBuilder): _root_.scala.Unit = $picklerLogic
          def tag: _root_.scala.pickling.FastTypeTag[$tpe] = $createTagTree
        }
        $picklerName
      }
    """
  }

  def reflectively(target: TermName, value: IrMember)(body: c.Tree => c.Tree): List[c.Tree] =
    sys.error("Reflective usage not implemented yet.")


  def computeType[T: c.WeakTypeTag]: Type = {
    val originalTpe = weakTypeOf[T]
    // Note: this makes it so modules work, things like foo.type.
    //       For some reason we get an issue with not having == defined on Class[_] otherwise.
    // TODO - fix this for certain primitive types, like Null, etc.
    if(originalTpe.termSymbol.isModule) originalTpe.widen
    else originalTpe
  }

}