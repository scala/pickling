package scala.pickling
package generator

/** This is the portion of code which can actually generate a pickler/unpickler
  * object instance from the IR AST.
  */
private[pickling]  trait SourceGenerator extends Macro with tags.FastTypeTagMacros {
  import c.universe._

  def pickleNull(builder: c.Tree): c.Tree =
    q"""_root_.scala.pickling.Defaults.nullPickler.pickle(null, $builder)"""



  def allowNonExistentField(impl: Tree): c.Tree = {
    q"""try $impl catch {
            case _: _root_.scala.pickling.PicklingException =>
            // TODO - Don't just ignore bad things, figure out how to actually read the class file for reals.
            // We could basically by blocked by scala here, though.
          }"""
  }
  def generatePickleImplFromAst(picklerAst: PicklerAst): c.Tree = {

    def genGetField(x: GetField): c.Tree = {
      def pickleLogic(isStaticallyElided: Boolean, fieldValue: Tree, tpe: Type): Tree = {
        val fieldPickler = c.fresh(newTermName("fieldPickler"))
        val elideHint =
          if (isStaticallyElided) q"b.hintElidedType($fieldPickler.tag)" else q""
        // NOTE; This will look up an IMPLICIT pickler for the value, based on the type.
        //       This is how we chain our macros to find all the valid types.
        q"""
           val $fieldPickler = _root_.scala.Predef.implicitly[_root_.scala.pickling.Pickler[$tpe]]
           $elideHint
           $fieldPickler.pickle($fieldValue, b)"""
      }

      def putField(getterLogic: Tree, isStaticallyElided: Boolean, tpe: Type): c.Tree =
        q"builder.putField(${x.name}, b => ${pickleLogic(isStaticallyElided, getterLogic, tpe)})"

      x.getter match {
        case x: IrField =>
          val tpe = x.tpe[c.universe.type](c.universe)
          val staticallyElided = tpe.isEffectivelyFinal || tpe.isEffectivelyPrimitive
          if(x.isScala || !x.isPublic) {
            // We always have to use reflection for scala fields right now.  Additionally, for Scala fields, we
            // actually have no idea if they exist at runtime, so we allow failure for now, which is EVIL, but we have no alternative.
            val result = reflectivelyGet(newTermName("picklee"), x)(fm => fm)
            val rTerm = c.fresh(newTermName("result"))
            val logic = q"""val $rTerm = { ..$result }
                            ${putField(q"$rTerm.asInstanceOf[$tpe]", staticallyElided, tpe)}"""
            if(x.isScala) allowNonExistentField(logic) else logic
          } else putField(q"picklee.${newTermName(x.fieldName)}", staticallyElided, tpe)
        case _: IrConstructor =>
          // This is a logic erorr
          sys.error(s"Pickling logic error.  Found constructor when trying to pickle field ${x.name}.")
        case y: IrMethod =>
          val tpe = y.returnType(c.universe)
          val staticallyElided = {
            // TODO - This is too naive of a determination.  In practice, we almost want an "always elide" and "never elide" mode.
            tpe.isEffectivelyFinal || tpe.isEffectivelyPrimitive
          }
          if (y.isPublic) putField(q"picklee.${newTermName(y.methodName)}", staticallyElided, tpe)
          else {
            val result = reflectivelyGet(newTermName("picklee"), y)(fm => putField(q"${fm}.asInstanceOf[${y.returnType(u).asInstanceOf[c.Type]}]", staticallyElided, tpe))
            q"""..$result"""
          }
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
        val throwUnknownTag =
          if(x.subClasses.isEmpty) {
            q"""throw _root_.scala.pickling.PicklingException("Class " + clazz + " not recognized by pickler")"""
        } else q"""throw _root_.scala.pickling.PicklingException("Class " + clazz + " not recognized by pickler, looking for one of: " + $dispatcheeNames)"""
        CaseDef(Bind(otherTermName, Ident(nme.WILDCARD)), throwUnknownTag)
      }
      val runtimeDispatch = CaseDef(Ident(nme.WILDCARD), EmptyTree, createRuntimePickler(q"builder"))
      val unknownDispatch =
        if(x.lookupRuntime) List(runtimeDispatch)
        else List(failDispatch)



      val picklerLookup = q"""
        val clazz = if (picklee != null) picklee.getClass else null
        ${Match(q"clazz", compileTimeDispatch ++ unknownDispatch)}
      """
      val subclasses =
        q"""
          val pickler: _root_.scala.pickling.Pickler[_] = $picklerLookup
          pickler.asInstanceOf[_root_.scala.pickling.Pickler[$tpe]].pickle(picklee, builder)
        """
      // If we have parent behavior, we need to do a quick instanceOf check first.
      x.parentBehavior match {
        case None => subclasses
        case Some(b) =>
          val parentTpe = x.parent.tpe[c.universe.type](c.universe)
          val impl = generatePickleImplFromAst(b)
          q"""if(classOf[$parentTpe] == picklee.getClass) $impl else $subclasses"""
      }
    }
    def genPickleEntry(op: PickleEntry): c.Tree = {
      val nested =
        op.ops.toList map genPickleOp
      val oid = c.fresh(newTermName("oid"))
      // TODO - hint known size
      val shareHint: List[c.Tree] =
        if(shareNothing) List(q"()")
        else List(q"val $oid = _root_.scala.pickling.internal.`package`.lookupPicklee(picklee)", q"builder.hintOid($oid)")
      /*val shareLogic: c.Tree =
        if(shareNothing) q"..$nested"
        else q"if($oid == -1) { ..$nested }"*/
      q"""
         ..$shareHint
         builder.beginEntry(picklee, tag)
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
        case x: PickleExternalizable => genExternalizablePickle(newTermName("picklee"), newTermName("builder"), x)
      }
    genPickleOp(picklerAst)
  }

  // Code which will lookup a runtime pickler.
  def createRuntimePickler(builder: c.Tree): c.Tree = q"""
    val classLoader = this.getClass.getClassLoader
    _root_.scala.pickling.internal.GRL.lock()
    val tag = try {
      _root_.scala.pickling.FastTypeTag.makeRaw(clazz)
    } finally _root_.scala.pickling.internal.GRL.unlock()
    _root_.scala.pickling.internal.`package`.currentRuntime.picklers.genPickler(classLoader, clazz, tag)
  """

  def createPickler(tpe: c.Type, builder: c.Tree): c.Tree = q"""
    val pickler = _root_.scala.Predef.implicitly[_root_.scala.pickling.Pickler[$tpe]]
    pickler
  """

  def checkNullPickle(pickleLogic: c.Tree): c.Tree =
    q"""
        if(null == picklee) {
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
    val unpicklerName    = c.fresh(newTermName("unpickler$unpickle$"))
    // TODO - is this the right place to do this?
    val staticHint       = if (tpe.isEffectivelyFinal) q"$readerName.hintElidedType($unpicklerName.tag)" else q"";

    val resultName = c.fresh(newTermName("result"))
    // TODO - may be able to drop locally.
      q"""
         _root_.scala.Predef.locally {
           val $readerName = reader.readField($name)
           var $unpicklerName: _root_.scala.pickling.Unpickler[$tpe] = null
           $unpicklerName = _root_.scala.Predef.implicitly[_root_.scala.pickling.Unpickler[$tpe]]
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

      case x: IrField =>
        val tpe = x.tpe[c.universe.type](c.universe)
        val read = readField(s.name, tpe)
        val staticallyElided = tpe.isEffectivelyFinal || tpe.isEffectivelyPrimitive
        if(x.isScala || !x.isPublic || x.isFinal) {
          reflectivelySet(newTermName("result"), x, read)
        } else q"""result.${newTermName(x.fieldName)} = $read"""
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


  def createUnpickler(tpe: Type): Tree =
    q"_root_.scala.Predef.implicitly[_root_.scala.pickling.Unpickler[$tpe]]"
  def genSubclassUnpickler(x: SubclassUnpicklerDelegation): c.Tree = {
    val tpe = x.parent.tpe[c.universe.type](c.universe)
    val defaultCase =
      if(x.lookupRuntime) CaseDef(Ident(nme.WILDCARD), EmptyTree, q"_root_.scala.pickling.internal.`package`.currentRuntime.picklers.genUnpickler(_root_.scala.pickling.internal.`package`.currentMirror, tagKey)")
      else CaseDef(Ident(nme.WILDCARD), EmptyTree, q"""throw new _root_.scala.pickling.PicklingException("Cannot unpickle, Unexpected tag: " + tagKey + " not recognized.")""")
    val subClassCases =
       x.subClasses.toList map { sc =>
         val stpe = sc.tpe[c.universe.type](c.universe)
         val skey = typeKey(stpe)
         CaseDef(Literal(Constant(skey)), EmptyTree, createUnpickler(stpe))
       }
    val subClass =
      q"""val unpickler: _root_.scala.pickling.Unpickler[_] = ${Match(q"tagKey", subClassCases ++ List(defaultCase))}
        unpickler.asInstanceOf[_root_.scala.pickling.Unpickler[$tpe]].unpickle(tagKey, reader)
        """
    x.parentBehavior match {
      case None => subClass
      case Some(p) =>
        val ptree = generateUnpickleImplFromAst(p)
        q"""if(tagKey == tag.key) $ptree else $subClass"""
    }
  }
  def genUnpickleSingleton(s: UnpickleSingleton): c.Tree = {
    val tpe = s.tpe.tpe[c.universe.type](c.universe)
    val m = tpe.typeSymbol.asClass.module
    q"$m"
  }

  def genAllocateInstance(x: AllocateInstance): c.Tree = {
    val tpe = x.tpe.tpe[c.universe.type](c.universe)
    q"""_root_.scala.concurrent.util.Unsafe.instance.allocateInstance(classOf[$tpe]).asInstanceOf[$tpe]"""
  }

  def generateUnpickleImplFromAst(unpicklerAst: UnpicklerAst): c.Tree = {
    unpicklerAst match {
      case c: CallConstructor => genConstructorUnpickle(c)
      case c: CallModuleFactory => genCallModuleFactory(c)
      case x: SetField => genSetField(x)
      case x: SubclassUnpicklerDelegation => genSubclassUnpickler(x)
      case x: UnpickleSingleton => genUnpickleSingleton(x)
      case x: AllocateInstance => genAllocateInstance(x)
        // TODO - This is kind of hacky, should be a temproary workaround for a better solution.
      case x: UnpickleExternalizable => genExternalizablUnPickle(newTermName("reader"), x)
      case x: UnpickleBehavior =>
        val behavior = x.operations.map(generateUnpickleImplFromAst).toList
        // TODO - This is kind of hacky.  We're trying to make sure during unpickling we always register/unregister appropriately...
        //  This is actually guarranteed to fail if "structural sharing" is done in a way such that the shared "tree"
        // of objects is needed to call the constructor. In this isntance, we are unable to register the new object
        // before constructing it, leading to out of order OIDs and a failed deserialize.
        // This is why we're deprecating sharing.
        x.operations match {
          case List() => q"null"
          case List(head: SubclassUnpicklerDelegation) => generateUnpickleImplFromAst(head)
            // TODO - Can we assume that ever additional operation is something which manipualtes the result?
            // Once again, flakyness here is why we're deprecating sharing.
          case hd :: tail =>
            val hdTree = generateUnpickleImplFromAst(hd)
            val tlTree = tail.map(generateUnpickleImplFromAst)
            // Note: We are *always* generating the unpickle sharing code, in the event that
            //       someone pickles with sharing, but we try to unpickle without it.
            q"""_root_.scala.Predef.locally {
                  val oid = _root_.scala.pickling.internal.`package`.preregisterUnpicklee()
                  val result = $hdTree;
                  _root_.scala.pickling.internal.`package`.registerUnpicklee(result, oid)
                  ..$tlTree;
                  result
                }"""
        }
    }
  }

  val picklingPath = q"_root_.scala.pickling"
  val picklersRegistry = q"$picklingPath.internal.currentRuntime.picklers"
  val generated = tq"$picklingPath.Generated"

  def picklerUnpicklerLazyInit(lookup1: c.Tree, lookup2: c.Tree, key: c.Tree,
                               tpe: c.Tree, notInitialized: c.TermName) = {
    q"""
      if($picklersRegistry.isLookupEnabled) {
        ($lookup1($key), $lookup2($key)) match {
          case (Some(p), Some(u)) if p == u => p.asInstanceOf[$tpe]
          case _ => $notInitialized: $tpe
        }
      } else $notInitialized: $tpe
    """
  }

  def generatePicklerUnpicklerClass[T: c.WeakTypeTag](impl: PickleUnpickleImplementation): c.Tree = {

    val tpe = computeType[T]
    val name = c.fresh(newTermName(syntheticBaseName(tpe) + "PicklerUnpickler"))
    val createTagTree = super[FastTypeTagMacros].impl[T]
    val unpickleLogic = genUnpicklerLogic[T](impl.unpickle)
    val pickleLogic = genPicklerLogic[T](impl.pickle)
    val key = q"$createTagTree.key"
    val picklerUnpicklerType = tq"$picklingPath.AbstractPicklerUnpickler[$tpe]"
    val genPicklerUnpicklerType = tq"$picklerUnpicklerType with $generated"
    val lookup1 = q"$picklersRegistry.lookupExistingPickler"
    val lookup2 = q"$picklersRegistry.lookupExistingUnpickler"

    q"""
       _root_.scala.Predef.locally {
          implicit object $name extends $picklerUnpicklerType with $generated
              with $picklingPath.AutoRegister[$tpe] {

            override lazy val tag: $picklingPath.FastTypeTag[$tpe] = $createTagTree
            override def pickle(picklee: $tpe, builder: $picklingPath.PBuilder): _root_.scala.Unit = $pickleLogic
            override def unpickle(tagKey: _root_.java.lang.String, reader: $picklingPath.PReader): _root_.scala.Any = $unpickleLogic

          }
          ${picklerUnpicklerLazyInit(lookup1, lookup2, key, genPicklerUnpicklerType, name)}
       }
     """

  }

  /** Given a tree which unpickles a value, we regsiter that value with its OID hint so that
    * we can effectively deserialized later references.
    */
  def registerUnPickledRef(instantiationLogic: c.Tree): c.Tree = {
    // TODO - We may not want to ALWAYS do this, some kind of enabling flag...
    val instance = c.fresh(newTermName("instance"))
    q"""
      val oid = _root_.scala.pickling.internal.`package`.currentRuntime.refRegistry.unpickle.preregisterUnpicklee()
      val $instance = $instantiationLogic
      _root_.scala.pickling.internal.`package`.currentRuntime.refRegistry.unpickle.registerUnpicklee($instance, oid)
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

  // -- Externalizable Hackery --
  def genExternalizablePickle(target: TermName, builder: TermName, pe: PickleExternalizable): c.Tree = {
    val out = c.fresh(newTermName("out"))
    val objectOutTpe = typeOf[scala.pickling.util.GenObjectOutput]
    val fieldName = "$ext"
    q"""val $out = new _root_.scala.pickling.util.GenObjectOutput
        $target.writeExternal($out)
        $builder.putField($fieldName, b =>
          _root_.scala.pickling.functions.pickleInto($out, b)
        )
     """
  }

  def genExternalizablUnPickle(reader: TermName, pe: UnpickleExternalizable): c.Tree = {
    val tpe = pe.tpe.tpe[c.universe.type](c.universe)
    val readerName = c.fresh(newTermName("readerName"))
    val target = c.fresh(newTermName("out"))
    val objectOutTpe = typeOf[scala.pickling.util.GenObjectOutput]
    val fieldName = "$ext"
    q"""
       val $target = _root_.scala.concurrent.util.Unsafe.instance.allocateInstance(classOf[$tpe]).asInstanceOf[$tpe]
       val $readerName = reader.readField($fieldName)
       val out = {
         val up = _root_.scala.Predef.implicitly[_root_.scala.pickling.Unpickler[$objectOutTpe]]
         up.unpickleEntry($readerName).asInstanceOf[$objectOutTpe]
       }
       val in = out.toInput
       $target.readExternal(in)
       $target
        """
  }

  // ---- Reflective Helper Methods ----

  def reflectivelyGet(target: TermName, value: IrMember)(body: c.Tree => c.Tree): List[c.Tree] = {
    // Note: We have chosen not to use scala reflection due to errors that occur in it.
    // TODO - Should we trap errors and return better error messages?
    // TODO - We should attempt to SAVE the reflective methods/fields somewhere so we aren't
    //        looking them up all the time.
    val valueTree =
      value match {
        case field: IrField =>
          val fieldTerm = c.fresh(newTermName("field"))
          val get = q"""
                val $fieldTerm = _root_.scala.pickling.internal.Reflect.getField($target.getClass, ${field.javaReflectionName})
                $fieldTerm.setAccessible(true)
                $fieldTerm.get($target)
                """
          get
        // Private scala methods may not encode normally for case classes.  This is a hack which goes after the field.
        // TODO - We should update this to look for the accessor method which scala generally exposes for the deconstructor.
        case mthd: IrMethod if mthd.isScala && mthd.isPrivate =>
          val methodTerm = c.fresh(newTermName("mthd"))
          // TODO - We may need to do a specialied lookup for magic named methods.
          q"""val $methodTerm = _root_.scala.pickling.internal.Reflect.getMethod($target.getClass, ${mthd.javaReflectionName}, Array())
              $methodTerm.setAccessible(true)
              $methodTerm.invoke($target)
              """
        case mthd: IrMethod =>
          val methodTerm = c.fresh(newTermName("mthd"))
          q"""val $methodTerm = _root_.scala.pickling.internal.Reflect.getMethod($target.getClass, ${mthd.javaReflectionName}, Array())
              $methodTerm.setAccessible(true)
              $methodTerm.invoke($target)"""
      }
    List(body(valueTree))
  }
  def reflectivelySet(target: TermName, setter: IrMember, value: c.Tree): c.Tree = {
    // Note: We've chosen not to use scala reflection due to it being buggy.
    // TODO - Should we trap errors and return better error messages?
    // TODO - We should attempt to SAVE the reflective methods/fields somewhere so we aren't
    //        looking them up all the time.
    setter match {
      case field: IrField =>
        val fieldTerm = c.fresh(newTermName("field"))
        val result = q"""
                 val $fieldTerm = _root_.scala.pickling.internal.Reflect.getField($target.getClass, ${field.javaReflectionName})
                 $fieldTerm.setAccessible(true)
                 $fieldTerm.set($target, ${liftPrimitives(value, field.tpe[c.universe.type](c.universe))})"""
        // Workaround for issues with not being able to accurate read scala symbols.
        if(field.isScala) allowNonExistentField(result) else result
      case mthd: IrMethod =>
        val methodTerm = c.fresh(newTermName("mthd"))
        // TODO - We should ensure types align.
        val List(List(tpe)) = mthd.parameterTypes[c.universe.type](c.universe)
        q"""
                val $methodTerm = _root_.scala.pickling.internal.Reflect.getMethod($target.getClass, ${mthd.javaReflectionName}, Array(classOf[$tpe]))
                $methodTerm.setAccessible(true)
                $methodTerm.invoke($target, ${liftPrimitives(value, tpe)})
              """
    }

  }

  // -- End Reflective Helper Methods --

}
