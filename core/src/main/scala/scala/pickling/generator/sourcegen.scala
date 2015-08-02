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
          if (isStaticallyElided) q"b.hintStaticallyElidedType()" else q""
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
        case _: IrConstructor =>
          // This is a logic erorr
          sys.error(s"Pickling logic error.  Found constructor when trying to pickle field ${x.name}.")
        case y: IrMethod =>
          val staticallyElided = {
            // TODO - Figure this out
            val tpe = y.returnType(c.universe)
            tpe.isEffectivelyFinal || tpe.isEffectivelyPrimitive
          }
          if (y.isPublic) putField(q"picklee.${newTermName(y.methodName)}", staticallyElided)
          else {
            val result = reflectivelyGet(newTermName("picklee"), y)(fm => putField(q"${fm}.asInstanceOf[${y.returnType(u).asInstanceOf[c.Type]}]", staticallyElided))
            q"""..$result"""
          }
      }
    }

    def reflectivelyGet(target: TermName, value: IrMember)(body: c.Tree => c.Tree): List[c.Tree] = {
      // TODO - Should we use scala reflection?
      // TODO - Should we trap errors and return better error messages?
      // TODO - We should attempt to SAVE the reflective methods/fields somewhere so we aren't
      //        looking them up all the time.
      val valueTree =
        value match {
          case field: IrField =>
            q"""
              _root_.scala.Predef.locally {
               val field = $target.getClass.getDeclaredField(${field.fieldName})
               field.setAccesible(true)
               field.get($target)
             }"""
          // Private scala methods may not encode normally for case classes.  This is a hack which goes after the field.
          // TODO - We should update this to look for the accessor method which scala generally exposes for the deconstructor.
          case mthd: IrMethod if mthd.isScala && mthd.isPrivate =>
            val fieldName = mthd.javaReflectionName
            // TODO - Check return type of method.
            q"""
              _root_.scala.Predef.locally {
               $target.getClass.getDeclaredMethods().find { m =>
                 (m.getName endsWith $fieldName) && (m.getParameterTypes.length == 0)
               } match {
                 // TODO - make sure mthd has no arguments
                 case Some(mthd)  =>
                   mthd.setAccessible(true)
                   mthd.invoke($target)
                 case None => _root_.scala.sys.error("Failed to reflectively find method: " + ${mthd.javaReflectionName})
               }

             }"""
          case mthd: IrMethod =>
            q"""_root_.scala.Predef.locally {
                  // TODO - make sure method has no arguments.
                  val mthd = $target.getClass.getDeclaredMethod(${mthd.javaReflectionName})
                  mthd.setAccessible(true)
                  mthd.invoke($target)
               }
             """
        }
      List(body(valueTree))
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
    def genPickleEntry(op: PickleEntry): c.Tree = {
      val nested =
        op.ops.toList map genPickleOp
      q"""
         builder.hintTag(tag)
         builder.beginEntry(picklee)
         $hintShareId
         ..$nested
         builder.endEntry()
       """
    }
    def genPickleOp(op: PicklerAst): c.Tree =
      op match {
        case PickleBehavior(ops) => q"""..${ops.map(genPickleOp)}"""
        case x: GetField => genGetField(x)
        case x: PickleEntry => genPickleEntry(x)
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
    checkNullPickle(generatePickleImplFromAst(picklerAst))
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
    val resultName = c.fresh(newTermName("result"))
    // TODO - may be able to drop locally.
      q"""
         _root_.scala.Predef.locally {
           val $readerName = reader.readField($name)
           var $unpicklerName: _root_.scala.pickling.Unpickler[$tpe] = null
           $unpicklerName = _root_.scala.Predef.implicitly[_root_.scala.pickling.Unpickler[$tpe]]
           $readerName.hintTag($unpicklerName.tag)
           $staticHint
           val typeString = $readerName.beginEntry()
           val $resultName = $unpicklerName.unpickle(typeString, $readerName)
           $readerName.endEntry()
           $resultName.asInstanceOf[$tpe]
         }
       """
  }


  def genConstructorUnpickle(cons: CallConstructor): c.Tree = {
    // Note, this is a bit ugly.
    var idx = 0
    val names = cons.fieldNames
    val tpess = cons.constructor.parameterTypes[c.universe.type](c.universe)
    val argss =
      tpess map { tpes =>
         tpes map { tpe =>
           val name = names(idx)
           idx += 1
           readField(name, tpe)
         }
      }
    val tpe = cons.constructor.returnType[c.universe.type](c.universe)
    // TODO - Handle reflective case.
    if(cons.requiresReflection) sys.error(s"Unable to reflectively call constructors, currently.")
    else {
      if(cons.constructor.parameterNames.isEmpty) q"""new ${tpe}"""
      else {
        q"new $tpe(...$argss)"
      }
    }
  }
  def genCallModuleFactory(cons: CallModuleFactory): c.Tree = {
    // Note, this is a bit ugly.
    var idx = 0
    val names = cons.fields
    val tpess = cons.factoryMethod.parameterTypes[c.universe.type](c.universe)
    val argss =
      tpess map { tpes =>
        tpes map { tpe =>
          val name = names(idx)
          idx += 1
          readField(name, tpe)
        }
      }
    val tpe = cons.factoryMethod.returnType[c.universe.type](c.universe)
    // TODO - Handle reflective case.
    val result = if(cons.requiresReflection) sys.error(s"Unable to reflectively call factory methods, currently.")
    else {
      if(cons.factoryMethod.parameterNames.isEmpty) q"${tpe}.${newTermName(cons.factoryMethod.methodName)}()"
      else {
        q"${tpe}.${newTermName(cons.factoryMethod.methodName)}(...$argss)"
      }
    }
    result
  }

  /** Creates a `set` operation that reads a value from the pickle and writes it into the object.
    *
    * Note: This assumes there exists a `result` name in scope which is the currently instantiated unpickle object.
    */
  def genSetField(s: SetField): c.Tree = {
    s.setter match {
      case x: IrMethod =>
        x.parameterTypes[c.universe.type ](c.universe) match {
          case List(List(tpe)) =>
            val read = readField(s.name, tpe)
            if(x.isPublic) {
              q"""
                 result.${newTermName(x.methodName)}($read)
               """
            } else reflectivelySet(newTermName("result"), x, read)
          case x => sys.error(s"Cannot handle a setting method that does not take exactly one parameter, found parameters: $x")
        }

      case x: IrField => sys.error(s"Unpickling to fields is not supported yet.")
    }

  }

  val ShortType = typeOf[Short]
  val CharType = typeOf[Char]
  val IntType = typeOf[Int]
  val LongType = typeOf[Long]
  val FloatType = typeOf[Float]
  val DoubleType = typeOf[Double]
  val BooleanType = typeOf[Boolean]
  /** This will lift any primitive value into  the java-boxed version (usefulf or reflective code.) */
  def liftPrimitives(value: c.Tree, tpe: Type): c.Tree = {
    tpe match {
      case ShortType => q"new _root_.java.lang.Short($value)"
      case CharType => q"new _root_.java.lang.Character($value)"
      case IntType => q"new _root_.java.lang.Integer($value)"
      case LongType => q"new _root_.java.lang.Long($value)"
      case FloatType => q"new _root_.java.lang.Float($value)"
      case DoubleType => q"new _root_.java.lang.Double($value)"
      case BooleanType => q"new _root_.java.lang.Boolean($value)"
      case _ => value
    }
  }

  def reflectivelySet(target: TermName, setter: IrMember, value: c.Tree): c.Tree = {
    // TODO - Should we use scala reflection?
    // TODO - Should we trap errors and return better error messages?
    // TODO - We should attempt to SAVE the reflective methods/fields somewhere so we aren't
    //        looking them up all the time.
      setter match {
        case field: IrField =>
          val fieldTerm = c.fresh(newTermName("field"))
          q"""
               val $fieldTerm = $target.getClass.getDeclaredField(${field.fieldName})
               $fieldTerm.setAccesible(true)
               $fieldTerm.set($target, ${liftPrimitives(value, field.tpe[c.universe.type](c.universe))})
          """
        case mthd: IrMethod =>
          val methodTerm = c.fresh(newTermName("mthd"))
          val valueTerm = c.fresh(newTermName("value"))
          // TODO - We should ensure types align.
          val List(List(tpe)) = mthd.parameterTypes[c.universe.type](c.universe)
          q"""$target.getClass.getDeclaredMethods.find { m =>
                (m.getName ==${mthd.javaReflectionName}) && (m.getParameterTypes.length == 1)
              } match {
                case Some(mthd) =>
                  val $valueTerm = ${liftPrimitives(value, tpe)}
                  mthd.setAccessible(true)
                  mthd.invoke($target, $valueTerm)
                case None =>
                  _root_.scala.sys.error("Could not find setter method: " + ${mthd.javaReflectionName})
             }
             """
      }

  }
  def createUnpickler(tpe: Type): Tree =
    q"_root_.scala.Predef.implicitly[_root_.scala.pickling.Unpickler[$tpe]]"
  def genSubclassUnpickler(x: SubclassUnpicklerDelegation): c.Tree = {
    val tpe = x.parent.tpe[c.universe.type](c.universe)
    // TODO - Allow runtime pickler lookup if enabled...
    val defaultCase =
      CaseDef(Ident(nme.WILDCARD), EmptyTree, q"""throw new Error(s"Cannot unpickle, Unexpected tag: $$tagKey")""")
    val subClassCases =
       x.subClasses.toList map { sc =>
         val stpe = sc.tpe[c.universe.type](c.universe)
         val skey = stpe.key
         CaseDef(Literal(Constant(skey)), EmptyTree, createUnpickler(stpe))
       }
    q"""val unpickler: _root_.scala.pickling.Unpickler[_] = ${Match(q"tagKey", subClassCases ++ List(defaultCase))}
      unpickler.asInstanceOf[_root_.scala.pickling.Unpickler[$tpe]].unpickle(tagKey, reader)
      """
  }
  def genUnpickleSingleton(s: UnpickleSingleton): c.Tree = {
    val tpe = s.tpe.tpe[c.universe.type](c.universe)
    val m = tpe.typeSymbol.asClass.module
    q"$m"
  }

  def generateUnpickleImplFromAst(unpicklerAst: UnpicklerAst): c.Tree = {
    unpicklerAst match {
      case c: CallConstructor => genConstructorUnpickle(c)
      case c: CallModuleFactory => genCallModuleFactory(c)
      case x: SetField => genSetField(x)
      case x: SubclassUnpicklerDelegation => genSubclassUnpickler(x)
      case x: UnpickleSingleton => genUnpickleSingleton(x)
      case x: UnpickleBehavior =>
        val behavior = x.operations.map(generateUnpickleImplFromAst).toList
        behavior match {
          case List() => q"null"
          case List(head) => head
            // TODO - Can we assume that ever additional operation is something which manipualtes the result?
          case hd :: tail =>
            q"""_root_.scala.Predef.locally { val result = $hd; ..$tail; result }"""
        }
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
          implicit object $unpicklerName extends  _root_.scala.pickling.Unpickler[$tpe] with _root_.scala.pickling.Generated {
            def unpickle(tagKey: _root_.java.lang.String, reader: _root_.scala.pickling.PReader): _root_.scala.Any = $unpickleLogic
            def tag: _root_.scala.pickling.FastTypeTag[$tpe] = $createTagTree
          }
          $unpicklerName : _root_.scala.pickling.Unpickler[$tpe]
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
          $name : _root_.scala.pickling.AbstractPicklerUnpickler[$tpe]
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




  def computeType[T: c.WeakTypeTag]: Type = {
    val originalTpe = weakTypeOf[T]
    // Note: this makes it so modules work, things like foo.type.
    //       For some reason we get an issue with not having == defined on Class[_] otherwise.
    // TODO - fix this for certain primitive types, like Null, etc.
    if(originalTpe.termSymbol.isModule) originalTpe.widen
    else originalTpe
  }

}