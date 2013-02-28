package scala.reflect.macros
package runtime

import scala.collection.mutable.{Set, Stack}
import scala.reflect.internal.Mode._
import scala.reflect.internal.SymbolTable
import scala.reflect.internal.util.NoSourceFile
import scala.tools.nsc.Global
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.Settings
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.util.ScalaClassLoader

object JitCompiler {
  def tryCompile(global: Global)(macroDef: global.Symbol, macroImpl: global.Symbol): Option[VirtualDirectory] = {
    try {
      // Step 1: Extract the transitive closure of the code referenced by macroImpl
      // Note that we create a dedicated instance of Global to do this in order not to corrupt `global`
      val closureAnalyzer = new { val mainGlobal: global.type = global } with ClosureAnalyzer(global.settings, global.reporter)
      val closure: List[closureAnalyzer.PackageDef] = closureAnalyzer.calculateClosure(macroDef, macroImpl)

      // Step 2: Compile the calculated closure into Java bytecodes very much like the REPL does
      // Note that we create a yet another instance of Global for this purpose
      val virtualDirectory = new VirtualDirectory("(macro jit)", None)
      val compilerSettings = global.settings.copy()
      compilerSettings.outputDirs setSingleOutput virtualDirectory
      val jitCompiler = new Global(compilerSettings, global.reporter)
      val importer = jitCompiler.mkImporter(closureAnalyzer)
      val units = closure.map(pdef => {
        def convertedPackage: jitCompiler.PackageDef = importer.importTree(closureAnalyzer.resetAllAttrs(pdef, resetAllTypeTrees = true)).asInstanceOf[jitCompiler.PackageDef]
        // def wrappedPackage = if (pdef.name == closureAnalyzer.nme.EMPTY_PACKAGE_NAME) convertedPackage.stats else List(convertedPackage)
        def wrappedPackage = convertedPackage.stats
        val cu = new jitCompiler.CompilationUnit(NoSourceFile)
        cu.body = jitCompiler.PackageDef(jitCompiler.Ident(pdef.name.toString), wrappedPackage)
        cu
      }).toList
      val numErrors    = jitCompiler.reporter.ERROR.count
      def hasNewErrors = jitCompiler.reporter.ERROR.count > numErrors
      val run = new jitCompiler.Run
      run.compileUnits(units, run.namerPhase)

      // Step 3: If the compilation succeeded, return the in-memory directory holding the classfiles
      if (!hasNewErrors) Some(virtualDirectory) else None
    } catch {
      case ex: MacroJitException =>
        global.analyzer.macroTraceJit("Unhandled MacroJitException: ")(ex)
        None
    }
  }

  abstract class ClosureAnalyzer(val globalSettings: Settings, reporter: Reporter) extends Global(globalSettings, reporter) with scala.reflect.macros.util.Traces {
    val mainGlobal: Global

    private val _run = new Run
    override def currentRun = _run
    globalPhase = currentRun.typerPhase

    override def forInteractive = true

    private lazy val fromMainGlobalImporter = new StandardImporter {
      val from: mainGlobal.type = mainGlobal

      protected lazy val completerMap = new Cache[Symbol, Tree]()
      override def redefineExistingModulesWithImportedOnes = true
      val debug = false

      @inline final def importerDebug(msg: => Any) { if (debug) println(msg) }

      override def recreateType(their: from.Type): Type = their match {
        case from.analyzer.ImportType(expr) => analyzer.ImportType(importTree(expr.asInstanceOf[from.Tree]))
        case _ => super.recreateType(their)
      }

      override def recreatedTreeCompleter(their: from.Tree, my: Tree): Unit = {
        super.recreatedTreeCompleter(their, my)
        attachCompleter(my, my.symbol, their.symbol)
      }

      private def attachCompleter(mytree: Tree, mysym: Symbol, theirsym: from.Symbol) = {
        importerDebug("!!!tryattachCompleter " + mytree.summaryString + " " +mysym + "#" + (if (mysym==null) "" else mysym.id), theirsym + "#" + (if (theirsym==null) "" else theirsym.id), theirsym != null && theirsym.isInitialized)
        if (mysym != null && mysym != NoSymbol && !theirsym.isInitialized)
          mytree match {
            case _: MemberDef => completerMap weakGet mysym match {
              case Some(result) => abort("cant attach completer again " + showRaw(mytree) + " " + mysym + "#" + mysym.id)
              case _ =>
                importerDebug("!!!attachCompleter", mysym + "#" + (if (mysym==null ) "" else mysym.id), theirsym + "#" + (if (theirsym==null) "" else theirsym.id), theirsym != null && theirsym.isInitialized)
                completerMap.weakUpdate(mysym, mytree)
            }
            // object O1 extends O2.C1 with O2.T1 with O3.T2[O4.C4] {
            // Select@7653(Ident@7650(O4), O4.C4)
            case _:Select =>
            // Annotation Tree Part jumps also here - do nothing
            // TODO: deny annotation parts make completers
            case _:Ident  =>
            case _:Apply  =>
            case _:Import =>
            case _ => abort("attachCompleter can't recognize " + showRaw(mytree))
          }
      }

      override def recreatedSymbolCompleter(theirsym: from.Symbol) = {
        val mytypeParams = theirsym.typeParams map importSymbol
        new LazyPolyType(mytypeParams) with FlagAgnosticCompleter {
          override def complete(mysym: Symbol) {
            importerDebug("!!!mkSymCompleter.complete " + mysym.id + " " + mysym + " " + completerMap.weakGet(mysym))
            val theircore = theirsym.info match {
              case from.PolyType(_, core) => core
              case core => core
            }
            mysym setInfo GenPolyType(mytypeParams, importType(theircore))
            mysym setAnnotations (theirsym.annotations map importAnnotationInfo)

            def processTParams(tparams: List[TypeDef], tparamsyms: List[Symbol]) =
              tparams zip tparamsyms foreach { case (tdef, tdefsym) => tdef.setSymbol(tdefsym) }

            def processVParams(vparamss: List[List[ValDef]], vparamssyms: List[List[Symbol]]) =
              vparamss.flatten zip vparamssyms.flatten foreach { case (vdef, vdefsym) => vdef.setSymbol(vdefsym) }

            def exportName(name: Name): from.Name =
              if (name.isTypeName) from.newTypeName(name.toString) else from.newTermName(name.toString)

            def processTemplateBody(body: List[Tree]) = body foreach {
              case EmptyTree => // Do nothing Generated by 'object A2_inobj {}'
              case mymdef: MemberDef =>
                val mymsym = mysym.info.decl(mymdef.name)
                val theirmsym = theirsym.info.decl(exportName(mymdef.name))
                assert(mymsym != NoSymbol && mymsym != null,s"member $mysym.$mymdef not found in imported tree")
                assert(theirmsym != NoSymbol && theirmsym != null,s"member $mysym.$mymdef not found in imported tree")
                mymdef.setSymbol(mymsym)
                // tparams processing
                mymdef match {
                  case TypeDef(_, _, tparams, _)    =>
                    processTParams(tparams, mymsym.typeParams)
                  case ClassDef(_, _, tparams, _)   =>
                    processTParams(tparams, mymsym.typeParams)
                  case DefDef(_,_,tparams,vparamss,_,_) =>
                    processTParams(tparams, mymsym.typeParams)
                    processVParams(vparamss, mymsym.paramss)
                  case _: ModuleDef | _: PackageDef | _: ValDef =>
                  case _ => abort("unsupported MdefChild " + mymdef.getClass)
                }

                // It' also possible that theirmsym symbol already initialized by somebody
                // without template initialisation this means that we should post parse tree here
                // and dont wait for completer
                if (theirmsym.isInitialized) {
                  mymdef match {
                    case myddef : DefDef => processRetType(myddef, mymsym)
                    case _ =>
                  }
                } else {
                  attachCompleter(mymdef, mymsym, theirmsym)
                }
              case stat =>
                abort("not implemented completer stat = "  + showRaw(stat))
            }

            def processRetType(ddef: DefDef, ddefsym: Symbol) = ddefsym.info match {
              case NullaryMethodType(resultType) =>
                if (ddef.tpt.isEmpty) ddef.tpt.defineType(resultType)
              case mt @ MethodType(params, resultType) =>
                if (ddef.tpt.isEmpty) ddef.tpt.defineType(resultType)
                processVParams(ddef.vparamss, mt.paramss)
              case PolyType(params,resultType) =>
                if (ddef.tpt.isEmpty) ddef.tpt.defineType(resultType)
              case _ =>
                abort("defdef with unknown type " + ddefsym.info.getClass)
            }

            completerMap weakGet mysym match {
              case Some(mytree) =>
                mytree match {
                  case _: ValDef =>
                  case ddef @ DefDef(_, _, _, _, _, _) =>
                    processRetType(ddef, mysym)
                  case ClassDef(_, _, _, Template(_, _, body)) =>
                    processTemplateBody(body)
                  case ModuleDef(_, _, Template(_, _, body)) =>
                    processTemplateBody(body)
                  case _: TypeDef =>
                  case _ =>
                    abort("not implemented "+ showRaw(mytree, printIds = true, printTypes = true) + " " + mytree.getClass)
                }
              case _ =>
            }
          }
        }
      }
    }

    private lazy val importedUnits: List[CompilationUnit] = mainGlobal.currentRun.units.map(unit => {
      val cu: CompilationUnit = new CompilationUnit(unit.source)
      val originalBody = mainGlobal.analyzer.original(unit.body)
      val preTypedBody = if (originalBody != null) originalBody else unit.body
      cu.body = fromMainGlobalImporter.importTree(preTypedBody)
      cu
    }).toList

    private type WalkPath = List[Int]
    private case class SymbolPath(symbol: Symbol, unit: CompilationUnit, ast: Tree, path: WalkPath) {
      def shorten = SymbolPath(symbol, unit, ast, path.tail)
      override def toString = "@" + symbol.toString + "#" + symbol.id + " p= " + path
    }

    private type SymbolSet = Set[SymbolPath]

    private abstract class PathWalker(wp: WalkPath, debug: Boolean = false) extends Traverser {
      var result: Option[Tree] = None

      private val path = new Stack[Int] ++ wp
      private var cIdx = path.pop

      protected def isLastStep = path.isEmpty && cIdx ==0
      def markNode(tree: Tree)
      def markUpperNode(tree: Tree)

      override def traverse(tree: Tree) =
        if (result.isEmpty) {
          if (debug)
            macroLogJit("PathWalker.traverse cIdx="  + cIdx + " path" + path + " " + tree.summaryString + "@" + tree.getClass)
          if (cIdx == 0) {
            if (path.isEmpty) {
              markNode(tree)
              result = Some(tree)
            } else {
              cIdx = path.pop
              markNode(tree)
              super.traverse(tree)
            }
          } else {
            cIdx -= 1
            if (debug)
              macroLogJit("PathWalker.markUpperNode cIdx="  + cIdx + " path" + path + " " + tree.summaryString + "@" + tree.getClass)
            markUpperNode(tree)
          }
        }
      }

      private class ContextCreator(unit: CompilationUnit, wp: WalkPath, debug: Boolean = false) extends PathWalker(wp, debug) {
        var context = analyzer.rootContext(unit, EmptyTree, false)
        def markNode(tree:Tree):Unit  = {
          assert(tree.symbol == NoSymbol || tree.symbol.isStatic, "ContextCreator Jumps in non static node: " + tree.symbol)
          tree match {
            case tree: Block                          =>
              abort("Illegal")
            case tree @ ModuleDef(_, _, impl) =>
              val sym = tree.symbol
              val clazz = sym.moduleClass

              // Self part
              context = context.makeNewScope(tree, clazz)
              analyzer.newNamer(context).enterSelf(impl.self)

              // Body part
              //context = context.make(impl,clazz,clazz.info.decls)
              context = context.makeNewScope(impl,clazz)

            case tree: PackageDef                     =>
              context = context.make(tree,tree.symbol.moduleClass,tree.symbol.info.decls)

            case tree: Import                       =>
              abort("Illegal")

            case tree: Template                     =>
              // Parsed already as ModuleDef children
            case _: TypeDef                          =>
              // Do Nothing
            case _: DefDef | _: ClassDef |  _: ValDef => // DefDef can be only last element in context maker - we can't and don't want to dive in function scope
              assert(isLastStep,"Illegal MemberDef in context creator: isLastStep=false")

            case _ => abort("ContextCreator_markNode:ERROR??? tree=" + showRaw(tree))
          }
      }

      def markUpperNode(tree: Tree) = tree match {
        case tree: Import =>
          context = context.makeNewImport(tree)
        case _ =>
      }
    }

    private class ExternalsMarker(inputs: SymbolSet, startSym: Symbol, skippedSet: Set[Symbol]) extends Traverser {
      val externalSyms = Set[Symbol]()

      // Symbols.scala hasTransOwner extension
      def hasTransOwnerForModule(sym0: Symbol, sym: Symbol): Boolean = {
        var o = sym0
        while ((o ne sym) && (o.companionSymbol ne sym) && (o ne NoSymbol)) o = o.owner
        (o eq sym) || (o.companionSymbol eq sym)
      }
      /* If we export ModuleDef, owner of its DefDefs is not module symbol
       we should check companionSymbol in owners list.
       if startSym is object - don't allow all internal members to be exported
       otherwise dont check object scope
       1 ) if in inputs is object AAA { def A1 = 1 } and our startSym is some
        def BBB = { AAA.A1 } we should produce AAA.A1 as external symbol - it can be used later as
        info about needed symbols for packing object AAA
        we should pack later object AAA with all it's members despite of members that refer to the macros
        and if some member call the macros and exists as "external" - this means 100% recursive macro call
        and we should produce error
       2 ) if we our startSym is object AAA we don't need info about it's content thats why we produce only
       object itself as external symbol
      */

      def isExternal: Symbol => Boolean = {
        def inputsCheck(s: Symbol) = inputs.exists(v => s.hasTransOwner(v.symbol)) || skippedSet.contains(s)
        def isExternal0(s: Symbol) = s != null && s != NoSymbol && !(
          inputsCheck(s) ||
          externalSyms.exists(s.hasTransOwner(_)) ||
          s.hasTransOwner(startSym)
        )
        def isModuleExternal(s:Symbol) = s != null && s != NoSymbol && !(
          inputsCheck(s) ||
          externalSyms.exists(s.hasTransOwner(_)) ||
          hasTransOwnerForModule(s,startSym)
        )
        startSym match {
          case _: ModuleClassSymbol => isModuleExternal
          case _ => isExternal0
        }
      }

      override def traverse(tree: Tree): Unit  = {
        def debugEnter(s: Symbol, t: Tree) = {
          macroLogJit("!!!ExternalsMarker.EnterSym " +
            "sym="+
            s+" "+
            s.id+" "+(if (s.isStatic ) "[STATIC]" else "")+
            s.getClass+
            " owner="+
            s.owner+" "+
            s.owner.id+" "+
            s.owner.getClass +
            " tree="+showRaw(tree, printIds = true) +
            " isExternal="  + isExternal(s) +
            " ifDefinedInTree="  + ifDefinedInTree(s)
          )
        }
        def flyToStatic(s: Symbol) = if (s.isStatic) s else s.owner
        def toModuleClass(s: Symbol) = if (s.isModuleClass) s.sourceModule else s
        def tryToEnter(s: Symbol) = tryToEnter0(flyToStatic(toModuleClass(s)))
        def tryToEnter0(s: Symbol) = if (isExternal(s) && ifDefinedInTree(s)) {
          debugEnter(s, tree)
          externalSyms += s
        }

        tree match {
          case _: Template | _: Apply | _: AppliedTypeTree => super.traverse(tree)
          case tpt:TypeTree =>
            if (tpt.original != null)
              traverse(tpt.original)
          case _ => tree.symbol match {
            case null | NoSymbol => super.traverse(tree)
            case s:Symbol =>
              tryToEnter(s)
              super.traverse(tree)
          }
        }
      }
    }

    private class PathFinder(sym: Symbol) extends Traverser {
      // Result is Tree , Path to this tree , isPrimary constructor (for defdef only
      //  - optimisation purposes - we dont need to traverse tree again)
      var result: Option[(Tree, WalkPath, Boolean)] = None
      private val path = new Stack[Int]
      private var cIdx = 0

      private var isPrimaryConstructor = false

      def isGetterOf(s1: Symbol,s2: Symbol) = s2 != null && s2 != NoSymbol && s1 != null && s1 != NoSymbol && s2.getter(s2.enclClass) == s1

      override def traverse(tree: Tree) = tree match {
        case _ if !result.isEmpty =>
        case ddef:DefDef if tree.symbol == sym =>
            result = Some((tree,(cIdx :: path.toList).reverse, ddef.name == nme.CONSTRUCTOR && isPrimaryConstructor))
        case _: ValDef if tree.symbol == sym || isGetterOf(sym, tree.symbol) => result = Some((tree,(cIdx :: path.toList).reverse, false))
        case _: ClassDef | _: TypeDef | _: ModuleDef if tree.symbol == sym => result = Some((tree, (cIdx :: path.toList).reverse, false))
        case _ =>
          tree match {
            case _: Template       => isPrimaryConstructor = true
            case DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => isPrimaryConstructor=false
            case _ =>
          }
          path.push(cIdx)
          cIdx = 0
          super.traverse(tree)
          if (result.isEmpty) cIdx = path.pop+1
      }
    }

    private def getAst(symbol: Symbol): Option[SymbolPath] = {
      def isGetterOf(s1: Symbol, s2: Symbol) = s2 != null && s2 != NoSymbol && s1 != null && s1 != NoSymbol && s2.getter(s2.enclClass) == s1

      def traverseUnits(units: List[CompilationUnit]): Option[SymbolPath] = units match  {
        case u :: last => traverseUnit(u) match {
          case None => traverseUnits(last)
          case ret @ Some(_)    => ret
        }
        case Nil => None
      }
      def mkPathTyper(u: CompilationUnit, path: WalkPath): analyzer.Typer  = {
        val cc = new ContextCreator(u, path, debug = false)
        cc.traverse(u.body)
        assert(cc.result.isDefined, "ContextCreator can not create context")
        analyzer.newTyper(cc.context.makeSilent(reportAmbiguousErrors = false))
      }
      def traverseUnit(u: CompilationUnit): Option[SymbolPath] = {
        val pf = new PathFinder(symbol)
        pf.traverse(u.body)
        pf.result match {
          case Some((ast: Tree, path: WalkPath, isPrimaryConstructor: Boolean)) =>
            ast match {
              case ModuleDef(_,_, impl) =>
                // ModuleDef as external element dont need full typing
                val tmpTyper = mkPathTyper(u, path)
                val typedAst = try {
                  tmpTyper.context.withMacrosDisabled {
                    tmpTyper.typedParentTypes(impl)
                  }
                } catch {
                  case ex: Throwable  =>
                    macroLogJit(s"!!!getAst warning: JitTyper Exception(notfatal) " + ex)
                    throw ex
                }
                Option(SymbolPath(ast.symbol, u, ast, path))
              case _: DefDef if isPrimaryConstructor  => // primary constructor
                macroLogJit("!!!getAst Skip Constructor typing rawAst=" + showRaw(ast))
                // Skip Constructor typing
                Option(SymbolPath(ast.symbol, u, ast, path))
              case _ =>
                val tmpTyper = mkPathTyper(u, path)

                macroLogJit("!!!getAst rawAst=" + showRaw(ast))

                val typedAst =
                  try tmpTyper.context.withMacrosDisabled(tmpTyper.typed(ast))
                  catch {
                    case ex: Throwable  =>
                      // Ignore Typer Errors
                      macroLogJit(s"!!!getAst warning: JitTyper Exception(notfatal) " + ex)
                      throw ex
                  }
                def filterNonFatalErrors(err: analyzer.AbsTypeError): Boolean = err match {
                  case _: analyzer.SymbolTypeError | _: analyzer.NormalTypeError =>
                    // TODO:
                    // 1 ) analyzer.NormalTypeError
                    //    check t4.scala enter existing symbols problem
                    // 2 ) analyzer.SymbolTypeError
                    //    [Type error at:funchain.scala,line-46,offset=737] self
                    //    constructor arguments cannot reference unconstructed `this`
                    false

                  case _ => true
                }
                val errorsToReport = tmpTyper.context.flushAndReturnBuffer()
                if (!errorsToReport.isEmpty) {
                  val fatalErrors = errorsToReport.filter(filterNonFatalErrors(_))
                  if (!fatalErrors.isEmpty ) {
                    macroLogJit("!!!macroLogJit typer errors: error[0].class=" + fatalErrors.head.getClass + " fatalErrors=" + fatalErrors)
                    throw new MacroJitException("getAst typer exception" + fatalErrors)
                  }
                } else if (typedAst exists (_.isErroneous)) {
                  // macro expand cancel dont generate errors in context
                  // only set typer.infer.setError
                  macroLogJit(s"!!!getAst typer warning" )
                  throw new MacroJitException("getAst typer inner exception" )
                }

                Option(SymbolPath(ast.symbol, u, typedAst, path))
            }
          case None => None
        }
      }
      macroLogJit("!!!getAst: search symbol=" + symbol + "#" + symbol.id)
      traverseUnits(importedUnits)
    }

    private def ifDefinedInTree(sym: Symbol) = (!sym.isErroneous) && (
      // TODO: sym.associatedFile can be null ORLY ????
      (sym.associatedFile != null) &&
      importedUnits.exists(unit => unit.source.file.canonicalPath == sym.associatedFile.canonicalPath)
    )

    /** Get clousure on toCheck Symbol list , with respect to forbiddenSet and checkedSyms
     * @param toCheck
     * @param checkedSyms
     * @param forbiddenSet
     * @param skippedSet - Just skip symbols ( getters to VAL )
     * @return SymbolSet - set of SymbolPath elements
     */
    private def getLinkedIds(toCheck: List[Symbol], checkedSyms: SymbolSet, forbiddenSet: Set[Symbol] = Set(), skippedSet: Set[Symbol] = Set()): SymbolSet = {
      toCheck match {
        case symSearch :: xs  => {
          macroLogJit("!!!getLinkedIds symSearch=" + symSearch +" forbiddenSet=" + forbiddenSet)
          getAst(symSearch) match {                                  // case ModuleDef() =>
            case Some(sp @ SymbolPath(symFound, unit, ast @ ModuleDef(mods, name, Template(parents, selfTree, body)), path)) =>
              macroLogJit("!!!getLinkedIds found ModuleDef ast=" + showRaw(ast))

              var forbiddenSet0 = forbiddenSet
              var checkedSyms0 = checkedSyms + sp

              def getExternals(ast: Tree): List[Symbol] = {
                val em = new ExternalsMarker(checkedSyms, symFound, skippedSet)
                em.traverse(ast)
                em.externalSyms.intersect(forbiddenSet).toList match {
                  case culprit :: _ => throw MacroJitCyclicClosureException(symSearch.toString, culprit.toString)
                  case Nil => em.externalSyms.toList
                }
              }

              val localParents:List[Symbol] = parents flatMap (getExternals(_)) filter (ifDefinedInTree(_))

              macroLogJit(">>>getLinkedIds_ModuleDef: Already Checked " + checkedSyms + " Check now: " + sp +
                " Parent Syms: " + localParents.map(s => s + "#" + s.id)
              )

              val skippedSet0: Set[Symbol] = skippedSet ++ localParents
              // Mark always needed symbols
              // 1 ) ovelroaded - we dont know exactly who calls them
              // 2 ) constuctor
              // TODO: Don't ignore Symbol with cycle but generate exception - to receive
              // error description on compilation time against "Symbol not found" or "False overriden usage"
              for (decl <- body if decl.symbol.name == nme.CONSTRUCTOR || (decl.symbol != NoSymbol && decl.symbol.isOverridingSymbol)) {
                try {
                  val ret = getLinkedIds(List(decl.symbol), checkedSyms0, forbiddenSet0, skippedSet0)
                  checkedSyms0 = ret
                } catch {
                  case _: MacroJitCyclicClosureException =>
                    forbiddenSet0 += decl.symbol
                }
              }

              macroLogJit("<<<getLinkedIds_ModuleDef: Current external syms " + checkedSyms0 + " forbiddenSet = " + forbiddenSet0)
              getLinkedIds(localParents ++ xs, checkedSyms0, forbiddenSet0, skippedSet)
            case Some(sp @ SymbolPath(symFound, unit, ast, path)) =>
              macroLogJit("!!!getLinkedIds found typed ast=" + showRaw(ast, printTypes = true))
              macroLogJit(">>>ExternalsMarkerCall: Start Already Checked " + checkedSyms + " Check now: " + sp)
              // ValDef support - if we found some symbol as symSearch filter ValDef element (symFound )
              // with its getter (symSearch)
              val skippedSet0 = if (symFound == symSearch) skippedSet else skippedSet + symSearch
              val em = new ExternalsMarker(checkedSyms, symFound, skippedSet0)
              em.traverse(ast)
              em.externalSyms.intersect(forbiddenSet).toList match {
                case culprit :: _ =>
                  throw MacroJitCyclicClosureException(symSearch.toString, culprit.toString)
                case Nil =>
                  // fly over not static symbols
                  val newToCheck = em.externalSyms.toList
                  macroLogJit("<<<ExternalsMarkerCall: Result New external syms " + newToCheck)
                  getLinkedIds(newToCheck ::: xs ,checkedSyms + sp,forbiddenSet,skippedSet0)
              }
            case None =>
              macroLogJit("!!!getLinkedIds Warn: Cant resolve ast for sym:" + symSearch + " sourceFile:" +symSearch.sourceFile + " ifDefinedInTree:" + ifDefinedInTree(symSearch))
              throw MacroJitCannotLocateDefTreeException(symSearch.toString)
              // TODO: Fatal error !
              getLinkedIds(xs, checkedSyms, forbiddenSet, skippedSet)
          }
        }
        case Nil      => checkedSyms
      }
    }

    private def packUnitElementsList(unit: CompilationUnit, startTrees: List[Tree], els: List[SymbolPath], startTree: Tree): Iterable[Tree] = {
      val simPaths = els.groupBy(e => if (e.path.isEmpty) -1 else e.path.head)
      val maxIndex = simPaths.keys.max
      val needConstructor = startTree.isInstanceOf[Template]
      macroLogJit(">>>packUnitElementsList class=" + startTree.getClass + " l=" + startTrees.length + " trees=" + startTrees.map(_.summaryString) + " els=" + els + " simPaths:" + simPaths)

      val ret = for ((tree,i) <- startTrees.zipWithIndex) yield {
        simPaths.get(i) match {
          case Some(el) => Some(packUnitElements(unit, tree, el.map(_.shorten)))
          case None => tree match {
            case _: Import if i < maxIndex => Some(tree)
            // nme.CONSTRUCTOR - is already here but for "input point" method objects are not in scope
            case DefDef(_, nme.CONSTRUCTOR, _, _, _, _) if needConstructor => Some(tree)
            case _ => None
          }
        }
      }

      macroLogJit("<<<packUnitElementsList" /*+ ret.flatten*/)
      ret.flatten
    }

    private def packUnitElements(unit: CompilationUnit, startTree: Tree, els: List[SymbolPath]): Tree = {
      macroLogJit(">>>packUnitElements u="+unit + " tree=" + startTree.summaryString + "@" + startTree.getClass + " els=" + els)
      assert(!els.isEmpty , "packUnitElements has empty input list")
      val pathEmpty = els.head.path.isEmpty

      val ret = startTree match {
        case PackageDef(pid, stats) =>
          PackageDef(pid, packUnitElementsList(unit, pid :: stats, els.map(_.shorten), startTree).toList)
        case ModuleDef(mods, name, impl) =>
          ModuleDef(mods, name, packUnitElementsList(unit, mods.annotations :+ impl, els, startTree).head.asInstanceOf[Template])
        case Template(parents, selftree, body) =>
          val childs = (if (selftree == emptyValDef) parents else parents :+ selftree) ++ body
          Template(parents, selftree, packUnitElementsList(unit, childs, els, startTree).toList)
          //startTree
        case _: DefDef if pathEmpty=>
          startTree
        case _ =>
          macroLogJit("Found not implemented " + startTree.summaryString)
          startTree
      }
      macroLogJit("<<<packUnitElements " + ret)
      ret
    }

    def calculateClosure(macroDef: mainGlobal.Symbol, macroImpl: mainGlobal.Symbol): List[PackageDef] = {
      // init exporter before other imports
      // order matters
      val forced = importedUnits

      // push whole symbol chain for the symbol being calculated
      // A1.A2.impl -> A1.sourceModule , A2.sourceModule , impl
      def toModuleClass(s: Symbol) = if (s.isModuleClass) s.sourceModule else s
      val startSym = fromMainGlobalImporter.importSymbol(macroImpl)
      val jitToCheck = startSym.ownerChain.takeWhile(!_.isPackageClass).map(toModuleClass(_))
      val closure = getLinkedIds(toCheck = jitToCheck, checkedSyms = Set(), forbiddenSet = Set(fromMainGlobalImporter.importSymbol(macroDef)))

      closure.groupBy(_.unit).map{ case (u, els) => packUnitElements(u, u.body, els.toList).asInstanceOf[PackageDef] }.toList
    }
  }
}
