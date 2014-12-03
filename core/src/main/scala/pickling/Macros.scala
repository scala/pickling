package scala.pickling

import scala.pickling.internal._
import ir._


trait TypeAnalysis extends Macro {
  import c.universe._

  def isCaseClass(sym: TypeSymbol): Boolean =
    sym.isClass && sym.asClass.isCaseClass

  def isClosed(sym: TypeSymbol): Boolean = {
    sym.isEffectivelyFinal || isCaseClass(sym) || {
      sym.isClass && {
        val classSym = sym.asClass
        classSym.isSealed && classSym.knownDirectSubclasses.forall(cl => isClosed(cl.asType))
      }
    }
  }
}

// purpose of this macro: implementation of genPickler[T]. i.e. the macro that is selected
// via implicit search and which initiates the process of generating a pickler for a given type T
// NOTE: dispatch is done elsewhere. picklers generated by genPickler[T] only know how to process T
// but not its subclasses or the types convertible to it!
trait PicklerMacros extends Macro with PickleMacros with FastTypeTagMacros {
  import c.universe._

  override def impl[T: c.WeakTypeTag]: c.Tree = preferringAlternativeImplicits {
    import definitions._
    val tpe = weakTypeOf[T]
    val sym = tpe.typeSymbol.asClass
    import irs._

    val primitiveSizes = Map(
      typeOf[Int] -> 4,
      typeOf[Short] -> 2,
      typeOf[Long] -> 8,
      typeOf[Double] -> 8,
      typeOf[Byte] -> 1,
      typeOf[Char] -> 2,
      typeOf[Float] -> 4,
      typeOf[Double] -> 8,
      typeOf[Boolean] -> 1
    )

    // uses "picklee"
    def getField(fir: FieldIR): Tree =
      if (fir.isPublic) q"picklee.${newTermName(fir.name)}"
      else if (fir.javaSetter.isDefined) {
        val getter = newTermName("get" + fir.name)
        q"picklee.$getter"
      } else reflectively("picklee", fir)(fm => q"$fm.get.asInstanceOf[${fir.tpe}]").head //TODO: don't think it's possible for this to return an empty list, so head should be OK

    def computeKnownSizeOfObjectOutput(cir: ClassIR): (Option[Tree], List[Tree]) = {
      // for now we cannot compute a fixed size for ObjectOutputs
      // in the future this will be a possible optimization (faster Externalizables)
      None -> List()
    }

    // this exists so as to provide as much information as possible about the size of the object
    // to-be-pickled to the picklers at runtime. In the case of the binary format for example,
    // this allows us to remove array copying and allocation bottlenecks
    // Note: this takes a "flattened" ClassIR
    // returns a tree with the size and a list of trees that have to be checked for null
    def computeKnownSizeIfPossible(cir: ClassIR): (Option[Tree], List[Tree]) = {
      if (cir.tpe <:< typeOf[Array[_]]) {
        val TypeRef(_, _, List(elTpe)) = cir.tpe
        val knownSize =
          if (elTpe.isEffectivelyPrimitive) Some(q"picklee.length * ${primitiveSizes(elTpe)} + 4")
          else None
        knownSize -> Nil
      } else if (tpe <:< typeOf[java.io.Externalizable]) {
        computeKnownSizeOfObjectOutput(cir) match {
          case (None, lst) => None -> List()
          case _ => c.abort(c.enclosingPosition, "not implemented")
        }
      } else {
        val possibleSizes: List[(Option[Tree], Option[Tree])] = cir.fields map {
          case fld if fld.tpe.isEffectivelyPrimitive =>
            val isScalar = !(fld.tpe <:< typeOf[Array[_]])
            if (isScalar) None -> Some(q"${primitiveSizes(fld.tpe)}")
            else {
              val TypeRef(_, _, List(elTpe)) = fld.tpe
              Some(getField(fld)) -> Some(q"${getField(fld)}.length * ${primitiveSizes(elTpe)} + 4")
            }
          case _ =>
            None -> None
        }

        val possibleSizes1 = possibleSizes.map(_._2)
        val resOpt =
          if (possibleSizes1.contains(None) || possibleSizes1.isEmpty) None
          else Some(possibleSizes1.map(_.get).reduce((t1, t2) => q"$t1 + $t2"))
        val resLst = possibleSizes.flatMap(p => if (p._1.isEmpty) List() else List(p._1.get))
        (resOpt, resLst)
      }
    }
    def unifiedPickle = { // NOTE: unified = the same code works for both primitives and objects
      val cir = newClassIR(tpe)
      // println(s"CIR for ${tpe.toString}: ${cir.fields.mkString(",")}")

      val hintKnownSize = computeKnownSizeIfPossible(cir) match {
        case (None, lst) => q""
        case (Some(tree), lst) =>
          val typeNameLen = tpe.key.getBytes("UTF-8").length
          val noNullTree  = lst.foldLeft[Tree](Literal(Constant(true)))((acc, curr) => q"$acc && ($curr != null)")
          q"""
            if ($noNullTree) {
              val size = $tree + $typeNameLen + 4
              builder.hintKnownSize(size)
            }
          """
      }
      val beginEntry = q"""
        $hintKnownSize
        builder.beginEntry(picklee)
      """
      val (nonLoopyFields, loopyFields) = cir.fields.partition(fir => !shouldBotherAboutLooping(fir.tpe))
      val putFields =
        if (tpe <:< typeOf[java.io.Externalizable]) {
          val fieldName = """$ext"""
          List(q"""
            val out = new scala.pickling.util.GenObjectOutput
            picklee.writeExternal(out)
            builder.putField($fieldName, b =>
              out.pickleInto(b)
            )
          """)
        } else (nonLoopyFields ++ loopyFields).flatMap(fir => {
        // for each field, compute a tree for pickling it
        // (or empty list, if impossible)

        def pickleLogic(fieldValue: Tree): Tree =
          if (fir.tpe.typeSymbol.isEffectivelyFinal) q"""
            b.hintStaticallyElidedType()
            $fieldValue.pickleInto(b)
          """ else q"""
            val subPicklee: ${fir.tpe} = $fieldValue
            if (subPicklee == null || subPicklee.getClass == classOf[${fir.tpe}]) b.hintDynamicallyElidedType()
            subPicklee.pickleInto(b)
          """

        def putField(getterLogic: Tree) =
          q"builder.putField(${fir.name}, b => ${pickleLogic(getterLogic)})"

        // we assume getterLogic is a tree of type Try[${fir.tpe}]
        def tryPutField(getterLogic: Tree) = {
          val tryName = c.fresh(newTermName("tr"))
          val valName = c.fresh(newTermName("value"))
          q"""
            val $tryName = $getterLogic
            if ($tryName.isSuccess) {
              val $valName = $tryName.get
              builder.putField(${fir.name}, b => ${pickleLogic(Ident(valName))})
            }
          """
        }

        if (sym.isModuleClass) {
          Nil
        } else if (fir.hasGetter) {
          if (fir.isPublic) List(putField(q"picklee.${newTermName(fir.name)}"))
          else reflectively("picklee", fir)(fm => putField(q"$fm.get.asInstanceOf[${fir.tpe}]"))
        } else if (fir.javaSetter.isDefined) {
          List(putField(getField(fir)))
        } else {
          reflectivelyWithoutGetter("picklee", fir)(fvalue =>
            tryPutField(q"$fvalue.asInstanceOf[scala.util.Try[${fir.tpe}]]"))
        }
      })
      val endEntry = q"builder.endEntry()"
      if (shouldBotherAboutSharing(tpe)) {
        q"""
          val oid = scala.pickling.internal.`package`.lookupPicklee(picklee)
          builder.hintOid(oid)
          $beginEntry
          if (oid == -1) { ..$putFields }
          $endEntry
        """
      } else {
        q"""
          $beginEntry
          ..$putFields
          $endEntry
        """
      }
    }

    //println("trying to generate pickler for type " + tpe.toString)

    def genClosedDispatch: Tree = {
      val clazzName = newTermName("clazz")
      val compileTimeDispatch = compileTimeDispatchees(tpe) filter (_ != NullTpe) map { subtpe =>
        CaseDef(Bind(clazzName, Ident(nme.WILDCARD)), q"clazz == classOf[$subtpe]", createPickler(subtpe, q"builder"))
      }
      q"""
        val clazz = if (picklee != null) picklee.getClass else null
        ${Match(q"clazz", compileTimeDispatch)}
      """
    }

    val pickleLogic: Tree = tpe.normalize match {
      case NothingTpe =>
        c.abort(c.enclosingPosition, "cannot generate pickler for type Nothing")

      case RefinedType(parents, decls) =>
        c.abort(c.enclosingPosition, "cannot generate pickler for refined type")

      // if it's a sealed abstract class or trait, following the pattern supported by staticOnly,
      // do not abort, but generate dispatch
      case tpe1 if sym.isAbstractClass && isClosed(sym) =>
        q"""
          val pickler: scala.pickling.SPickler[_] = $genClosedDispatch
          pickler.asInstanceOf[scala.pickling.SPickler[$tpe1]].pickle(picklee, builder)
        """

      case _ if sym.isClass =>
        // if class is abstract return instance of `PicklerUnpicklerNotFound`.
        // this triggers the generation of a dispatch based on the runtime class of the picklee.
        if (sym.asClass.isAbstractClass) {
          //println("abstract class, returning PicklerUnpicklerNotFound")
          return q"new scala.pickling.PicklerUnpicklerNotFound[$tpe]"
        } else
          unifiedPickle

      case _ =>
        c.abort(c.enclosingPosition, s"cannot generate pickler for type $tpe")
    }

    val createTagTree = super[FastTypeTagMacros].impl[T]

    val picklerName = c.fresh(syntheticPicklerName(tpe).toTermName)
    q"""
      implicit object $picklerName extends scala.pickling.SPickler[$tpe] with scala.pickling.Generated {
        import scala.pickling._
        import scala.pickling.internal._
        import scala.pickling.`package`.PickleOps
        def pickle(picklee: $tpe, builder: scala.pickling.PBuilder): Unit = $pickleLogic
        def tag: FastTypeTag[$tpe] = $createTagTree
      }
      $picklerName
    """
  }

  def dpicklerImpl[T: c.WeakTypeTag]: c.Tree = {
    val tpe = weakTypeOf[T]
    val picklerName = c.fresh((syntheticBaseName(tpe) + "DPickler"): TermName)
    val dpicklerPickleImpl = pickleWithTagInto(q"picklee0", q"builder")

    q"""
      implicit object $picklerName extends scala.pickling.DPickler[$tpe] {
        import scala.pickling._
        import scala.pickling.internal._
        import scala.pickling.`package`.PickleOps
        def pickle(picklee0: $tpe, builder: scala.pickling.PBuilder): Unit = $dpicklerPickleImpl
      }
      $picklerName
    """
  }
}

import HasCompat._

/*
 * This macro generates an unpickler for an _abstract_ type that is also an open sum.
 */
trait OpenSumUnpicklerMacro extends Macro with UnpicklerMacros with FastTypeTagMacros {
  override def impl[T: c.WeakTypeTag]: c.Tree = preferringAlternativeImplicits {
    import c.universe._
    import compat._
    import definitions._
    val tpe = weakTypeOf[T]
    val targs = tpe match { case TypeRef(_, _, targs) => targs; case _ => Nil }
    val sym = tpe.typeSymbol.asClass
    import irs._

    val cancel: () => Nothing =
      () => c.abort(c.enclosingPosition, s"cannot unpickle $tpe")

    def dispatchLogic = {
      val compileTimeDispatch = createCompileTimeDispatch(tpe)
      val refDispatch         = createRefDispatch()
      val runtimeDispatch     = CaseDef(Ident(nme.WILDCARD), EmptyTree, q"""
        val tag = scala.pickling.FastTypeTag(typeString)
        scala.pickling.Unpickler.genUnpickler(reader.mirror, tag)
      """)

      q"""
        ${Match(q"typeString", compileTimeDispatch :+ refDispatch :+ runtimeDispatch)}
      """
    }

    val unpickleLogic = tpe match {
      case NullTpe => cancel()
      case NothingTpe => cancel()
      case _ if tpe.isEffectivelyPrimitive || sym == StringClass => cancel()
      case _ if sym.isAbstractClass =>
        if (isClosed(sym)) {
          cancel()
        } else {
          // generate runtime dispatch
          val dispUnpicklerName = newTermName("unpickler$dispatch$")
          q"""
            val typeString = tag.key
            val $dispUnpicklerName = $dispatchLogic
            $dispUnpicklerName.unpickle(tag, reader)
          """
        }
      case _ => cancel()
    }

    val createTagTree = super[FastTypeTagMacros].impl[T]
    val unpicklerName = c.fresh(syntheticUnpicklerName(tpe).toTermName)

    q"""
      implicit object $unpicklerName extends scala.pickling.Unpickler[$tpe] with scala.pickling.Generated {
        def unpickle(tag: => scala.pickling.FastTypeTag[_], reader: scala.pickling.PReader): Any = $unpickleLogic
        def tag: scala.pickling.FastTypeTag[$tpe] = $createTagTree
      }
      $unpicklerName
    """
  }
}

// purpose of this macro: implementation of genUnpickler[T]. i.e., the macro that is selected via implicit
// search and which initiates the process of generating an unpickler for a given type T.
// NOTE: dispatch is done elsewhere. unpicklers generated by genUnpickler[T] only know how to process T
// but not its subclasses or the types convertible to it!
trait UnpicklerMacros extends Macro with UnpickleMacros with FastTypeTagMacros {
  override def impl[T: c.WeakTypeTag]: c.Tree = preferringAlternativeImplicits {
    import c.universe._
    import compat._
    import definitions._
    val tpe = weakTypeOf[T]
    val targs = tpe match { case TypeRef(_, _, targs) => targs; case _ => Nil }
    val sym = tpe.typeSymbol.asClass
    import irs._
    def unpicklePrimitive = q"reader.readPrimitive()"
    def unpickleObject = {
      def readField(name: String, tpe: Type) = {
        val readerName = c.fresh(newTermName("reader"))
        val readerUnpickleTree = readerUnpickle(tpe, readerName)
        q"""
          val $readerName = reader.readField($name)
          $readerUnpickleTree
        """
      }

      if (tpe <:< typeOf[java.io.Externalizable]) {
        val fieldName = """$ext"""
        val readerName = c.fresh(newTermName("reader"))
        val objectOutTpe = typeOf[scala.pickling.util.GenObjectOutput]
        val readerUnpickleTree = readerUnpickleTopLevel(objectOutTpe, readerName)
        q"""
          val inst = scala.concurrent.util.Unsafe.instance.allocateInstance(classOf[$tpe]).asInstanceOf[$tpe]
          val $readerName = reader.readField($fieldName)
          val out = $readerUnpickleTree
          val in = out.toInput
          inst.readExternal(in)
          inst
        """
      } else {
      // TODO: validate that the tpe argument of unpickle and weakTypeOf[T] work together
      // NOTE: step 1) this creates an instance and initializes its fields reified from constructor arguments
      val cir = newClassIR(tpe)
      val isPreciseType = targs.length == sym.typeParams.length && targs.forall(_.typeSymbol.isClass)

      val canCallCtor = cir.canCallCtor

      // STEP 2: remove transient fields from the "pending fields", the fields that need to be restored.

      // TODO: for ultimate loop safety, pendingFields should be hoisted to the outermost unpickling scope
      // For example, in the snippet below, when unpickling C, we'll need to move the b.c assignment not
      // just outside the constructor of B, but outside the enclosing constructor of C!
      //   class С(val b: B)
      //   class B(var c: C)
      // TODO: don't forget about the previous todo when describing the sharing algorithm for the paper
      // it's a very important detail, without which everything will crumble
      // no idea how to fix that, because hoisting might very well break reading order beyond repair
      // so that it becomes hopelessly unsync with writing order
      // nevertheless don't despair and try to prove whether this is or is not the fact
      // i was super scared that string sharing is going to fail due to the same reason, but it did not :)
      // in the worst case we can do the same as the interpreted runtime does - just go for allocateInstance

      // pending fields are fields that are restored after instantiation (e.g., through field assignments)
      val pendingFields = if (!canCallCtor) cir.fields else cir.fields.filter(fir =>
        fir.isNonParam || shouldBotherAboutLooping(fir.tpe) || fir.javaSetter.isDefined
      )

      val instantiationLogic = {
        if (sym.isModuleClass) {
          q"${sym.module}"
        } else if (cir.javaGetInstance) {
          q"""java.lang.Class.forName(${tpe.toString}).getDeclaredMethod("getInstance").invoke(null)"""
        } else if (canCallCtor) {
          val ctorFirs = cir.fields.filter(_.param.isDefined)
          val ctorSig: Map[Symbol, Type] = ctorFirs.map(fir => (fir.param.get: Symbol, fir.tpe)).toMap

          if (ctorSig.isEmpty) {
            q"new $tpe"
          } else {
            val ctorSym = ctorSig.head._1.owner.asMethod
            val ctorArgs = ctorSym.paramss.map(_.map(f => {
              val delayInitialization = pendingFields.exists(_.param.map(_ == f).getOrElse(false))
              if (delayInitialization) q"null" else readField(f.name.toString, ctorSig(f))
            }))
            q"new $tpe(...$ctorArgs)"
          }

        } else {
          q"scala.concurrent.util.Unsafe.instance.allocateInstance(classOf[$tpe]).asInstanceOf[$tpe]"
        }
      }
      // NOTE: step 2) this sets values for non-erased fields which haven't been initialized during step 1
      val initializationLogic = {
        if (sym.isModuleClass || pendingFields.isEmpty) {
          instantiationLogic
        } else {
          val instance = newTermName(tpe.typeSymbol.name + "Instance")

          val initPendingFields = pendingFields.flatMap(fir => {
            val readFir = readField(fir.name, fir.tpe)
            if (fir.isPublic && fir.hasSetter) List(q"$instance.${newTermName(fir.name)} = $readFir".asInstanceOf[Tree])
            else if (fir.javaSetter.isDefined) {
              val JavaProperty(name, declaredIn, isAccessible) = fir.javaSetter.get
              if (!isAccessible) {
                val methodName = "set" + name
                // obtain Class of parameter
                val className = fir.tpe.toString
                val (classTree, readTree) = className match {
                  case "Byte"    => (q"java.lang.Byte.TYPE",      q"new java.lang.Byte($readFir)")
                  case "Short"   => (q"java.lang.Short.TYPE",     q"new java.lang.Short($readFir)")
                  case "Char"    => (q"java.lang.Character.TYPE", q"new java.lang.Character($readFir)")
                  case "Int"     => (q"java.lang.Integer.TYPE",   q"new java.lang.Integer($readFir)")
                  case "Long"    => (q"java.lang.Long.TYPE",      q"new java.lang.Long($readFir)")
                  case "Float"   => (q"java.lang.Float.TYPE",     q"new java.lang.Float($readFir)")
                  case "Double"  => (q"java.lang.Double.TYPE",    q"new java.lang.Double($readFir)")
                  case "Boolean" => (q"java.lang.Boolean.TYPE",   q"new java.lang.Boolean($readFir)")
                  case _         => (q"Class.forName($className)", readFir)
                }
                List(q"""
                  val paramClass = $classTree
                  val method = Class.forName($declaredIn).getDeclaredMethod($methodName, paramClass)
                  method.setAccessible(true)
                  method.invoke($instance, $readTree)
                """)
              } else {
                val setter = newTermName("set" + name)
                List(q"$instance.$setter($readFir)")
              }
            } else if (fir.accessor.isEmpty) List(q"""
              try {
                val javaField = $instance.getClass.getDeclaredField(${fir.name})
                javaField.setAccessible(true)
                javaField.set($instance, $readFir)
              } catch {
                case e: java.lang.NoSuchFieldException => /* do nothing */
              }
            """)
            else reflectively(instance, fir)(fm => q"$fm.set($readFir)".asInstanceOf[Tree])
          })

          if (shouldBotherAboutSharing(tpe)) {
            q"""
              val oid = scala.pickling.internal.`package`.preregisterUnpicklee()
              val $instance = $instantiationLogic
              scala.pickling.internal.`package`.registerUnpicklee($instance, oid)
              ..$initPendingFields
              $instance
            """
          } else {
            q"""
              val $instance = $instantiationLogic
              ..$initPendingFields
              $instance
            """
          }         }
      }
      q"$initializationLogic"
      }
    }

    val unpickleLogic = tpe match {
      case NullTpe => q"null"
      case NothingTpe => c.abort(c.enclosingPosition, "cannot unpickle Nothing") // TODO: report the deserialization path that brought us here
      case _ if tpe.isEffectivelyPrimitive || sym == StringClass => unpicklePrimitive
      case _ if sym.isAbstractClass =>
        if (isClosed(sym)) {
          val dispatchLogic = Match(q"tag.key", createCompileTimeDispatch(tpe) :+ createRefDispatch())
          q"""
            val unpickler: scala.pickling.Unpickler[_] = $dispatchLogic
            unpickler.asInstanceOf[scala.pickling.Unpickler[$tpe]].unpickle(tag, reader)
          """
        } else {
          c.abort(c.enclosingPosition, s"cannot unpickle $tpe")
        }
      case _ =>
        q"""
          if (tag.key == scala.pickling.FastTypeTag.Null.key) {
            null
          } else if (tag.key == scala.pickling.FastTypeTag.Ref.key) {
            val refUnpickler = ${createUnpickler(RefTpe)}
            refUnpickler.unpickle(tag, reader)
          } else if (tag.key == ${if (tpe <:< typeOf[Singleton]) sym.fullName + ".type" else tpe.key}) {
            $unpickleObject
          } else {
            val rtUnpickler = scala.pickling.Unpickler.genUnpickler(reader.mirror, tag)
            rtUnpickler.unpickle(tag, reader)
          }
        """
    }

    val createTagTree = super[FastTypeTagMacros].impl[T]

    val unpicklerName = c.fresh(syntheticUnpicklerName(tpe).toTermName)
    q"""
      implicit object $unpicklerName extends scala.pickling.Unpickler[$tpe] with scala.pickling.Generated {
        import scala.language.existentials
        import scala.pickling._
        import scala.pickling.ir._
        import scala.pickling.internal._
        def unpickle(tag: => scala.pickling.FastTypeTag[_], reader: scala.pickling.PReader): Any = $unpickleLogic
        def tag: FastTypeTag[$tpe] = $createTagTree
      }
      $unpicklerName
    """
  }
}

// purpose of this macro: implementation of PickleOps.pickle and pickleInto. i.e., this exists so as to:
// 1) perform dispatch based on the type of the argument
// 2) insert a call in the generated code to the genPickler macro (described above)
trait PickleMacros extends Macro with TypeAnalysis {
  import c.universe._
  import definitions._

  def pickleTo[T: c.WeakTypeTag](output: c.Tree)(format: c.Tree): c.Tree = {
    val tpe = weakTypeOf[T]
    val q"${_}($pickleeArg)" = c.prefix.tree
    val endPickle = if (shouldBotherAboutCleaning(tpe)) q"clearPicklees()" else q"";
    val pickleeName = newTermName("picklee$pickleTo$")
    val builderName = newTermName("builder$pickleTo$")
    q"""
      import scala.pickling._
      import scala.pickling.internal._
      val $pickleeName: $tpe = $pickleeArg
      val $builderName = $format.createBuilder($output)
      $pickleeName.pickleInto($builderName)
      $endPickle
    """
  }

  def pickle[T: c.WeakTypeTag](format: c.Tree): c.Tree = {
    val tpe = weakTypeOf[T]
    val q"${_}($pickleeArg)" = c.prefix.tree
    val endPickle = if (shouldBotherAboutCleaning(tpe)) q"clearPicklees()" else q"";
    val pickleeName = newTermName("picklee$pickle$")
    val builderName = newTermName("builder$pickle$")
    q"""
      import scala.pickling._
      import scala.pickling.internal._
      val $pickleeName: $tpe = $pickleeArg
      val $builderName = $format.createBuilder()
      $pickleeName.pickleInto($builderName)
      $endPickle
      $builderName.result()
    """
  }

  def createPickler(tpe: c.Type, builder: c.Tree): c.Tree = q"""
    val pickler = implicitly[scala.pickling.SPickler[$tpe]]
    $builder.hintTag(pickler.tag)
    pickler
  """

  def createRuntimePickler(builder: c.Tree): c.Tree = q"""
    val classLoader = this.getClass.getClassLoader
    val tag = scala.pickling.FastTypeTag.mkRaw(clazz, scala.reflect.runtime.universe.runtimeMirror(classLoader))
    $builder.hintTag(tag)
    scala.pickling.SPickler.genPickler(classLoader, clazz, tag)
  """

  def genDispatchLogic(tpe: c.Type, builder: c.Tree, pickleeName: c.TermName): c.Tree = {
    val sym = tpe.typeSymbol

    def nonFinalDispatch = {
      val clazzName = newTermName("clazz")
      val compileTimeDispatch = compileTimeDispatchees(tpe) filter (_ != NullTpe) map (subtpe =>
        CaseDef(Bind(clazzName, Ident(nme.WILDCARD)), q"clazz == classOf[$subtpe]", createPickler(subtpe, builder))
      )
      //TODO OPTIMIZE: do getClass.getClassLoader only once
      val runtimeDispatch = CaseDef(Ident(nme.WILDCARD), EmptyTree, createRuntimePickler(builder))
      // TODO: do we still want to use something like HasPicklerDispatch?
      q"""
        val customPickler = implicitly[scala.pickling.SPickler[$tpe]]
        if (customPickler.isInstanceOf[scala.pickling.PicklerUnpicklerNotFound[_]] || customPickler.isInstanceOf[scala.pickling.Generated]) {
          val clazz = if ($pickleeName != null) $pickleeName.getClass else null
          ${Match(q"clazz", compileTimeDispatch :+ runtimeDispatch)}
        } else {
          // without Generated we would arrive here in two cases:
          // 1. we have found a custom pickler that can handle the abstract type tpe
          // 2. we have generated a pickler for a concrete superclass!
          // in the 2nd case we still have to do the dispatch!
          $builder.hintTag(customPickler.tag)
          customPickler
        }
      """
    }

    def refinedDispatch(parentTpe: Type) = {
      val inferred = c.inferImplicitValue(appliedType(typeOf[SPickler[_]].typeConstructor, List(tpe)))
      if (inferred == EmptyTree) {
        c.abort(c.enclosingPosition, s"could not find implicit pickler for refined type: $tpe")
      } else {
        q"""
          $builder.hintTag($inferred.tag)
          $inferred
        """
      }
    }

    // NOTE: this has zero effect on performance...
    // def listDispatch = {
    //   val List(nullTpe, consTpe, nilTpe) = compileTimeDispatchees(tpe)
    //   q"""
    //     import scala.language.existentials
    //     if (picklee eq Nil) ${createPickler(nilTpe, builder)}
    //     else if (picklee eq null) ${createPickler(nullTpe, builder)}
    //     else ${createPickler(consTpe, builder)}
    //   """
    // }
    // if (sym == ListClass) listDispatch else

    if (c.inferImplicitValue(typeOf[IsStaticOnly]) != EmptyTree) {
      if (!isClosed(sym.asType))
        c.abort(c.enclosingPosition, "cannot generate fully static pickler")
    }

    if (sym.asType.isAbstractType || sym.isEffectivelyFinal) createPickler(tpe, builder)
    else tpe.normalize match {
      case RefinedType(parents, _) => refinedDispatch(parents.head)
      case _ => nonFinalDispatch
    }
  }

  /** Used by the main `pickle` macro. Its purpose is to pickle the object that it's called on *into* the
   *  the `builder` which is passed to it as an argument.
   */
  def pickleInto[T: c.WeakTypeTag](builder: c.Tree): c.Tree = {
    val q"${_}($pickleeArg)" = c.prefix.tree
    pickleWithTagInto(pickleeArg, builder)
  }

  def pickleWithTagInto[T: c.WeakTypeTag](picklee: c.Tree, builder: c.Tree): c.Tree = {
    val tpe = weakTypeOf[T].widen // to make module classes work
    val sym = tpe.typeSymbol

    val pickleeName = newTermName("picklee$pickleInto$")
    val picklerName = newTermName("pickler$pickleInto$")

    val picklingLogic = if (sym.isClass && sym.asClass.isPrimitive) q"""
      val $picklerName = ${createPickler(tpe, builder)}
      $picklerName.pickle($pickleeName, $builder)
    """ else q"""
      if ($pickleeName != null) {
        val $picklerName = ${genDispatchLogic(tpe, builder, pickleeName)}
        $picklerName.asInstanceOf[scala.pickling.SPickler[$tpe]].pickle($pickleeName, $builder)
      } else {
        $builder.hintTag(scala.pickling.FastTypeTag.Null)
        scala.pickling.SPickler.nullPicklerUnpickler.pickle(null, $builder)
      }
    """

    q"""
      import scala.language.existentials
      import scala.pickling._
      import scala.pickling.internal._
      val $pickleeName: $tpe = $picklee
      scala.pickling.internal.GRL.lock()
      $picklingLogic
      scala.pickling.internal.GRL.unlock()
    """
  }
}

// purpose of this macro: implementation of unpickle method of class UnpickleOps, which:
// 1) dispatches to the correct unpickler based on the type of the input;
// 2) inserts a call in the generated code to the genUnpickler macro (described above)
trait UnpickleMacros extends Macro with TypeAnalysis {
  import c.universe._

  // TODO: currently this works with an assumption that sharing settings for unpickling are the same as for pickling
  // of course this might not be the case, so we should be able to read settings from the pickle itself
  // this is not going to be particularly pretty. unlike the fix for the runtime interpreter, this fix will be a bit of a shotgun one
  def pickleUnpickle[T: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    val pickleArg = c.prefix.tree
    val readerName = newTermName("reader$unpickle$")
    val readerUnpickleTree = readerUnpickleTopLevel(tpe, readerName)
    val formatName = newTermName("format$unpickle$")
    val pickleName = newTermName("pickle$unpickle$")
    q"""
      import scala.language.existentials
      import scala.pickling._
      import scala.pickling.internal._
      val $formatName = implicitly[scala.pickling.PickleFormat]
      val $pickleName = $pickleArg.thePickle.asInstanceOf[$formatName.PickleType]
      val $readerName = $formatName.createReader($pickleName, scala.pickling.internal.`package`.currentMirror)
      $readerUnpickleTree
    """
  }

  def readerUnpickle(tpe: Type, readerName: TermName): Tree =
    readerUnpickleHelper(tpe, readerName)(false)

  def readerUnpickleTopLevel(tpe: Type, readerName: TermName): Tree =
    readerUnpickleHelper(tpe, readerName)(true)

  def createUnpickler(tpe: Type): Tree =
    q"implicitly[scala.pickling.Unpickler[$tpe]]"

  def createRefDispatch(): CaseDef =
    CaseDef(Literal(Constant(FastTypeTag.Ref.key)), EmptyTree, createUnpickler(typeOf[refs.Ref]))

  def createCompileTimeDispatch(tpe: Type): List[CaseDef] = {
    compileTimeDispatchees(tpe) map (subtpe => {
      // TODO: do we still want to use something like HasPicklerDispatch (for unpicklers it would be routed throw tpe's companion)?
      CaseDef(Literal(Constant(subtpe.key)), EmptyTree, createUnpickler(subtpe))
    })
  }

  def readerUnpickleHelper(tpe: Type, readerName: TermName)(isTopLevel: Boolean = false): Tree = {
    val staticHint       = if (tpe.typeSymbol.isEffectivelyFinal && !isTopLevel) q"$readerName.hintStaticallyElidedType()" else q""
    val unpickleeCleanup = if (isTopLevel && shouldBotherAboutCleaning(tpe)) q"clearUnpicklees()" else q""
    val unpicklerName    = c.fresh(newTermName("unpickler$unpickle$"))
    q"""
      var $unpicklerName: scala.pickling.Unpickler[$tpe] = null
      $unpicklerName = implicitly[scala.pickling.Unpickler[$tpe]]
      scala.pickling.internal.GRL.lock()
      $readerName.hintTag($unpicklerName.tag)
      $staticHint
      val typeString = $readerName.beginEntryNoTag()
      val result = $unpicklerName.unpickle({ scala.pickling.FastTypeTag(typeString) }, $readerName)
      $readerName.endEntry()
      $unpickleeCleanup
      scala.pickling.internal.GRL.unlock()
      result.asInstanceOf[$tpe]
    """
  }
}
