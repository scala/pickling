package scala.pickling
package generator

import scala.reflect.api.Universe

/** This is the portion of code which can actually generate a pickler/unpickler object instance
  * from the IR AST.
  *
  *
  */
trait SourceGenerator extends Macro with FastTypeTagMacros {
  import c.universe._

  def pickleNull(builder: c.Tree): c.Tree =
    q"""$builder.hintTag(_root_.scala.pickling.FastTypeTag.Null)
        _root_.scala.pickling.Defaults.nullPickler.pickle(null, $builder)"""

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
    def genSubclassDispatch(x: SubclassDispatch): c.Tree = {
      val tpe = x.parent.tpe[c.universe.type](c.universe)
      val clazzName = newTermName("clazz")
      val compileTimeDispatch: List[CaseDef] = (x.subClasses map { subtpe =>
        val tpe = subtpe.tpe[c.universe.type](c.universe)
        CaseDef(Bind(clazzName, Ident(nme.WILDCARD)), q"clazz == classOf[$tpe]", createPickler(tpe, q"builder"))
      })(collection.breakOut)

      val failDispatch = {
        val dispatcheeNames = x.subClasses.map(_.className).mkString(", ")
        val otherTermName = newTermName("other")
        val throwUnknownTag = q"""throw _root_.scala.pickling.PicklingException("Class " + other + " not recognized by pickler, looking for one of: " + $dispatcheeNames)"""
        CaseDef(Bind(otherTermName, Ident(nme.WILDCARD)), throwUnknownTag)
      }
      // val runtimeDispatch = CaseDef(Ident(nme.WILDCARD), EmptyTree, createRuntimePickler(q"builder"))
      // TODO - Figure out if we can handle runtime dispatch...
      val unknownDispatch = List(failDispatch)


      val picklerLookup = q"""
        val clazz = if (picklee != null) picklee.getClass else null
        ${Match(q"clazz", compileTimeDispatch ++ unknownDispatch)}
      """
      q"""
          val pickler: _root_.scala.pickling.Pickler[_] = $picklerLookup
          pickler.asInstanceOf[_root_.scala.pickling.Pickler[$tpe]].pickle(picklee, builder)
        """
    }
    def genPickleOp(op: PicklerAst): c.Tree =
      op match {
        case PickleBehavior(ops) => q"""..${ops.map(genPickleOp)}"""
        case x: GetField => genGetField(x)
        case x: SubclassDispatch => genSubclassDispatch(x)
      }
    genPickleOp(picklerAst)
  }

  def createPickler(tpe: c.Type, builder: c.Tree): c.Tree = q"""
    val pickler = _root_.scala.Predef.implicitly[_root_.scala.pickling.Pickler[$tpe]]
    $builder.hintTag(pickler.tag)
    pickler
  """

  def hintShareId: c.Tree = {
    // TODO - Some kind of flag to disable this logic from being generated.
    q"""val oid = _root_.scala.pickling.internal.`package`.lookupPicklee(picklee)
        builder.hintOid(oid)"""
  }

  def checkNullPickle(pickleLogic: c.Tree): c.Tree =
    q"""
        if(null == picklee) {
          builder.hintTag(_root_.scala.pickling.FastTypeTag.Null)
          _root_.scala.pickling.Defaults.nullPickler.pickle(null, builder)
        } else $pickleLogic"""

  def genPicklerLogic[T: c.WeakTypeTag](picklerAst: PicklerAst): c.Tree = {
    // TODO - The pickler logic should actually check to make sure the thing passed is actually a FOO, and
    //        If it is not, we delegate to a runtime pickler.
    //        ALTHOUGH, we could, instead, delegate that as function of the AST that we can encode, so
    //        The presence of runtime code is controlled by generation algorithms, rather than the code generator.
    //        We could still disable the generation here.
    //        This is somethign the current algorithm does which we do not do.

    // TODO - We may not want to ALWAYS serialize null as null....
    checkNullPickle(q"""
          builder.hintTag(tag)
          builder.beginEntry(picklee)
          $hintShareId
          ${generatePickleImplFromAst(picklerAst)}
          builder.endEntry()""")
  }

  def genUnpicklerLogic[T: c.WeakTypeTag](unpicklerAst: UnpicklerAst): c.Tree = {
    // TODO - Make sure this lines up with existing unpickler logic
    val unpickleLogic = generateUnpickleImplFromAst(unpicklerAst)
    // Handle null + Ref types first
    unpickleNull(unpickleRef(unpickleLogic))
  }

  /** Wraps unpickle logic with the logic on how to handle `null` values.   Assumes the term `tagKey` is available. */
  def unpickleNull(unpickleLogic: c.Tree): c.Tree = {
    q""" if (tagKey == _root_.scala.pickling.FastTypeTag.Null.key) null else $unpickleLogic"""
  }
  /** Wraps unpickle logic with the logic on how to handle `Ref` values.   Assumes the terms `tagKey` and `reader` are available. */
  def unpickleRef(unpickleLogic: c.Tree): c.Tree = {
    q"""if (tagKey == _root_.scala.pickling.FastTypeTag.Ref.key) {
         _root_.scala.Predef.implicitly[_root_.scala.pickling.Unpickler[_root_.scala.pickling.refs.Ref]].unpickle(tagKey, reader)
       } else $unpickleLogic"""
  }

  def readField(name: String, tpe: Type): c.Tree = {
    val readerName = c.fresh(newTermName("reader"))
    // TODO - is this the right place to do this?
    val staticHint       = if (tpe.isEffectivelyFinal) q"$readerName.hintStaticallyElidedType()" else q"";
    val unpicklerName    = c.fresh(newTermName("unpickler$unpickle$"))
      q"""
         _root_.scala.Predef.locally {
           val $readerName = reader.readField($name)
           var $unpicklerName: _root_.scala.pickling.Unpickler[$tpe] = null
           $unpicklerName = _root_.scala.Predef.implicitly[_root_.scala.pickling.Unpickler[$tpe]]
           $readerName.hintTag($unpicklerName.tag)
           $staticHint
           val typeString = $readerName.beginEntry()
           val result = $unpicklerName.unpickle(typeString, $readerName)
           $readerName.endEntry()
           result.asInstanceOf[$tpe]
         }
       """
  }


  def genConstructorUnpickle(cons: CallConstructor): c.Tree = {
    val args = (cons.fieldNames zip cons.constructor.parameterTypes[c.universe.type](c.universe)).map {
      case (name, tpe) => readField(name, tpe)
    }.toList
    val tpe = cons.constructor.returnType[c.universe.type](c.universe)
    // TODO - Handle reflective case.
    if(cons.requiresReflection) sys.error(s"Unable to reflectively call constructors, currently.")
    else {
      if(cons.constructor.parameterNames.isEmpty) q"""new ${tpe}"""
      else {
        q"new $tpe(...$args)"
      }
    }

  }

  def generateUnpickleImplFromAst(unpicklerAst: UnpicklerAst): c.Tree = {
    unpicklerAst match {
      case c: CallConstructor => genConstructorUnpickle(c)
      case x: CallSingletoneFactory => ???
      case x: CallStaticFactory => ???
      case x: SetField => ???
      case x: UnpickleBehavior =>
        val behavior = x.operations.map(generateUnpickleImplFromAst).toList
        q"..$behavior"
    }
  }


  /** generates the tree which will construct + return a new instance of a Pickler class, capable of
    * pickling an instance of type T, using the behavior outlined by the PicklerAst.
    */
  def generatePicklerClass[T: c.WeakTypeTag](picklerAst: PicklerAst): c.Tree = {
    val tpe = computeType[T]
    val picklerName = c.fresh((syntheticBaseName(tpe) + "Pickler"): TermName)
    val createTagTree = super[FastTypeTagMacros].impl[T]
    q"""
      _root_.scala.Predef.locally {
        implicit object $picklerName extends _root_.scala.pickling.Pickler[$tpe] with _root_.scala.pickling.Generated {
          def pickle(picklee: $tpe, builder: _root_.scala.pickling.PBuilder): _root_.scala.Unit = ${genPicklerLogic[T](picklerAst)}
          def tag: _root_.scala.pickling.FastTypeTag[$tpe] = $createTagTree
        }
        $picklerName
      }
    """
  }

  def generateUnpicklerClass[T: c.WeakTypeTag](unpicklerAst: UnpicklerAst): c.Tree = {
    val tpe = computeType[T]
    val unpicklerName = c.fresh((syntheticBaseName(tpe) + "Unpickler"): TermName)
    val createTagTree = super[FastTypeTagMacros].impl[T]
    val unpickleLogic = genUnpicklerLogic[T](unpicklerAst)
    q"""
       _root_.scala.Predef.locally {
          implicit object $unpicklerName extends  _root_.scala.pickling.Unpickler[Foo] with _root_.scala.pickling.Generated {
            def unpickle(tagKey: _root_.java.lang.String, reader: _root_.scala.pickling.PReader): _root_.scala.Any = $unpickleLogic
            def tag: _root_.scala.pickling.FastTypeTag[$tpe] = $createTagTree
          }
          $unpicklerName
       }
     """
  }

  def generatePicklerUnpicklerClass[T: c.WeakTypeTag](impl: PickleUnpickleImplementation): c.Tree = {
    val tpe = computeType[T]
    val name = c.fresh((syntheticBaseName(tpe) + "PicklerUnpickler"): TermName)
    val createTagTree = super[FastTypeTagMacros].impl[T]
    val unpickleLogic = genUnpicklerLogic[T](impl.unpickle)
    val pickleLogic = genPicklerLogic[T](impl.pickle)
    q"""
       _root_.scala.Predef.locally {
          implicit object $name extends _root_.scala.pickling.AbstractPicklerUnpickler[$tpe] with _root_.scala.pickling.Generated {
            //import _root_.scala.language.existentials
            override def pickle(picklee: $tpe, builder: _root_.scala.pickling.PBuilder): _root_.scala.Unit = $pickleLogic
            override def unpickle(tagKey: _root_.java.lang.String, reader: _root_.scala.pickling.PReader): _root_.scala.Any = $unpickleLogic
            override def tag: _root_.scala.pickling.FastTypeTag[$tpe] = $createTagTree
          }
          $name
       }
     """
  }


  /** Given a tree which unpickles a value, we regsiter that value with its OID hint so that
    * we can effectively deserialized later references.
    */
  def registerUnPickledRef(instantiationLogic: c.Tree): c.Tree = {
    // TODO - We may not want to ALWAYS do this, some kind of enabling flag...
    val instance = c.fresh(newTermName("instance"))
    // TODO - Rather than using the package object, we should grab the 'current runtime' interface and use that.
    q"""
              val oid = _root_.scala.pickling.internal.`package`.preregisterUnpicklee()
              val $instance = $instantiationLogic
              _root_.scala.pickling.internal.`package`.registerUnpicklee($instance, oid)
              $instance
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