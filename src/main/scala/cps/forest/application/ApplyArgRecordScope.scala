package cps.forest.application

import scala.annotation.tailrec
import scala.quoted._


import cps._
import cps.forest._
import cps.misc._


trait ApplyArgRecordScope[F[_], CT]:

  thisTreeTransform: TreeTransformScope[F,CT] =>

  import qctx.tasty._


  sealed trait ApplyArgRecord:
    def term: Term
    def index: Int
    def identArg: Term
    def isAsync: Boolean
    def hasShiftedLambda: Boolean
    def noOrderDepended: Boolean
    def useIdent: Boolean = isAsync || !noOrderDepended
    def isOrderDepended = !noOrderDepended
    def shift(): ApplyArgRecord
    def append(tree: CpsTree): CpsTree

  case class ApplyArgRepeatRecord(
       term: Repeated,
       index: Int,
       elements: List[ApplyArgRecord],
  ) extends ApplyArgRecord {
    override def useIdent: Boolean = (elements.exists(x => x.isAsync || x.isOrderDepended))
    override def identArg: Term =
      if (useIdent)
          Repeated(elements.map(_.identArg),term.elemtpt)
      else if (hasShiftedLambda)
          Repeated(elements.map(_.identArg),shiftedLambdaTypeTree(term.elemtpt))
      else
          term
    override def isAsync = elements.exists(_.isAsync)
    override def hasShiftedLambda = elements.exists(_.hasShiftedLambda)
    override def noOrderDepended = elements.forall(_.noOrderDepended)
    override def shift() = copy(elements = elements.map(_.shift()))

    override def append(tree: CpsTree): CpsTree =
       if (elements.isEmpty)
        tree
       else
         elements.foldRight(tree){(e,s) =>
           e match
            case _: ApplyArgNoPrecalcTermRecord =>
                  s
            case e1: ApplyArgPrecalcTermRecord =>
                  e.append(s)
            case _: ApplyArgLambdaRecord => s
            case _ => // incorrect warning
               throw MacroError("Impossible: repeated inside repeated",cpsCtx.patternCode)
         }


  }

  case class ApplyArgNoPrecalcTermRecord(
       term: Term,
       index: Int
  ) extends ApplyArgRecord
  {
     def isAsync = false
     def hasShiftedLambda: Boolean = false
     def noOrderDepended = termIsNoOrderDepended(term)
     def identArg: Term = term
     def shift(): ApplyArgRecord = this
     override def append(tree: CpsTree): CpsTree = tree
  }

  case class ApplyArgPrecalcTermRecord(
       term: Term,
       index: Int,
       termCpsTree: CpsTree,
       valDef: ValDef,
       ident: Term
  ) extends ApplyArgRecord
  {
     def isAsync = termCpsTree.isAsync
     def hasShiftedLambda: Boolean = false
     def noOrderDepended = termIsNoOrderDepended(term)
     def identArg: Term = ident
     def shift(): ApplyArgRecord = this
     override def append(tree: CpsTree): CpsTree =
        ValCpsTree(valDef, termCpsTree, tree)
  }


  case class ApplyArgLambdaRecord(
       term: Block,   // Lambda,  see coding of Lambda in Tasty Reflect.
       index: Int,
       cpsBody: CpsTree,
       shifted: Boolean
  ) extends ApplyArgRecord {

       def hasShiftedLambda: Boolean = cpsBody.isAsync

       def isAsync: Boolean = false

       def noOrderDepended: Boolean = true

       def identArg: Term =
         if (hasShiftedLambda || shifted)
            val (params, body) = term match
              case Lambda(params, body) => (params, body)
              case _ =>
                 throw MacroError(s"Lambda expexted, we have ${term.asExpr.show}",term.asExpr)
            term.tpe match
              case MethodType(paramNames, paramTypes, resType) =>
                  val mt = shiftedMethodType(paramNames, paramTypes, resType)
                  createAsyncLambda(mt, params)
              case ft@AppliedType(tp,tparams) =>
                  if (ft.isFunctionType) {
                      val paramTypes = tparams.dropRight(1).map(typeOrBoundsToType(_,false))
                      val resType = typeOrBoundsToType(tparams.last,true)
                      val paramNames = params.map(_.name)
                      val mt = shiftedMethodType(paramNames, paramTypes, resType)
                      createAsyncLambda(mt, params)
                  } else if (tp <:< partialFunctionType ) {
                      val (tIn, tOut) = tparams match
                         case tIn::tOut::Nil => (tIn, tOut)
                         case _ =>
                           throw MacroError(s"PartialFunction should have 2 type parameters, we have $tparams", term.asExpr)
                      val matchTerm = body match
                         case m@Match(_,_) => m
                         case _ =>
                           throw MacroError(s"PartialFunction should be represented as Match term, we have $body", posExprs(body,term))
                      createAsyncPartialFunction(tIn, tOut, matchTerm, params)
                  } else {
                      throw MacroError(s"FunctionType expected, we have ${tp}", term.asExpr)
                  }
              case other =>
                  // TODO: logging compiler interface instead println
                  println(s"MethodType expected, we have ${term.tpe}")
                  println(s"term.show = ${term.show}")
                  println(s"term.body = ${term}")
                  println(s"mt = ${other}")
                  throw MacroError(s"methodType expected for ${term.asExpr.show}, we have $other",term.asExpr)
         else
            term

       def shift(): ApplyArgRecord = copy(shifted=true)

       def append(a: CpsTree): CpsTree = a

       private def createAsyncPartialFunction(from: TypeOrBounds,
                                                to: TypeOrBounds,
                                                body: Match,
                                                params: List[ValDef]): Term =
         val toInF = typeInMonad(to)
         val fromType = typeOrBoundsToType(from)
         val matchVar = body.scrutinee
         val paramNames = params.map(_.name)

         def newCheckBody(inputVal:Term):Term =

            val casePattern = '{
                 ${inputVal.asExpr} match
                    case _ => false
            }.asTerm

            @tailrec
            def transformCases(rest:List[CaseDef],
                               acc:List[CaseDef],
                               wasDefault: Boolean):List[CaseDef]=
              rest match
                case h::t =>
                     val nh = rebindCaseDef(h, Literal(Constant(true)), Map.empty, false)
                     transformCases(t, nh::acc, wasDefault)
                case Nil =>
                      val lastCase = casePattern match
                        case Inlined(_,List(),Match(x,cases)) => cases.head
                        case Match(x,cases) => cases.head
                        case _ =>
                            throw MacroError("Internal error: case pattern should be Inlined(_,_,Match) pr Match, we have $casePattern",posExpr(term))
                      ;
                      (lastCase::acc).reverse

            Match.copy(body)(inputVal, transformCases(body.cases,Nil,false))
            //Match(inputVal, transformCases(body.cases,Nil,false))

         def newCheck(): Term =
            val mt = MethodType(paramNames)(_ => List(fromType), _ => defn.BooleanType)
            Lambda(mt, args => changeArgs(params,args,newCheckBody(matchVar)))

         def newBody():Term =
            val mt = MethodType(paramNames)( _ => List(fromType), _ => toInF)
            createAsyncLambda(mt, params)

         def termCast[E](term: Term, tp:quoted.Type[E]): Expr[E] =
            term.asExpr.asInstanceOf[Expr[E]] // FIXME use `term.asExprOf[E]` to have a cheked cast

         /*
          // blocked by
         val helper = '{ cps.runtime.PartialFunctionHelper }.asTerm
         val helperSelect = Select.unique(helper,"create")
         val checkLambda = newCheck()
         val bodyLambda = newBody()

         val createPF = Apply(
                          TypeApply(helperSelect,List(Inferred(fromType),Inferred(toInF))),
                          List(checkLambda, bodyLambda)
                        )
         val r = createPF
         */
         val r = fromType.seal match
           case '[$ft] =>
             toInF.seal match
               case '[$tt] =>
                  '{ new PartialFunction[$ft,$tt] {
                       override def isDefinedAt(x1:$ft):Boolean =
                          ${ newCheckBody('x1.asTerm ).asExprOf[Boolean] }
                       override def apply(x2:$ft): $tt =
                          ${ val nBody = cpsBody.transformed
                             nBody match
                               case m@Match(scr,caseDefs) =>
                                 val b0 = Map(matchVar.symbol -> 'x2.asTerm)
                                 val nCaseDefs = caseDefs.map( cd =>
                                                    rebindCaseDef(cd, cd.rhs, b0, true))
                                 val nTerm = Match.copy(m)('x2.asTerm, nCaseDefs)
                                 termCast(nTerm,tt)
                               case _ =>
                                 throw MacroError(
                                   s"assumed that transformed match is Match, we have $nBody",
                                   posExprs(term)
                                 )
                           }
                     }
                   }.asTerm
               case _ =>
                  throw MacroError("Can't skolemize $toInF", posExprs(term) )
           case _ =>
             throw MacroError("Can't skolemize $fromType", posExprs(term) )

         r

       private def createAsyncLambda(mt: MethodType, params: List[ValDef]): Term =
         val transformedBody = cpsBody.transformed
         Lambda(mt, args => changeArgs(params,args,transformedBody))

       private def rebindCaseDef(caseDef:CaseDef,
                                 body: Term,
                                 assoc: Map[Symbol, Term],
                                 processBody: Boolean): CaseDef = {

         def rebindPatterns(pattern: Tree, map:Map[Symbol,Term]): (Tree, Map[Symbol, Term]) = {
           pattern match
             case bd@Bind(name,pat1) =>
                val bSym = bd.symbol
                val nBSym = Symbol.newBind(rootContext.owner,name,bSym.flags, Ref(bSym).tpe.widen)
                val nMap = map.updated(bSym, Ref(nBSym))
                val (nPat1, nMap1) = rebindPatterns(pat1, nMap)
                (Bind(nBSym,nPat1), nMap1)
             case u@Unapply(fun, implicits, patterns) =>
                val s0: (List[Tree], Map[Symbol,Term]) = (List.empty, Map.empty)
                val sR = patterns.foldLeft(s0){ (s,e) =>
                   val (ep, em) = rebindPatterns(e,s._2)
                   (ep::s._1, em)
                }
                val nPatterns = sR._1.reverse
                (Unapply.copy(u)(fun, implicits, nPatterns), sR._2)
             case other =>
                (other, map)
         }

         val (nPattern, newBindings) = rebindPatterns(caseDef.pattern, assoc)
         val nGuard = caseDef.guard.map( changeSyms(newBindings, _ ) )
         val nBody = if (processBody) changeSyms(newBindings, body) else body
         CaseDef(nPattern, nGuard, nBody)
       }

       private def changeArgs(params:List[ValDef], nParams:List[Tree], body: Term): Term =
         val association: Map[Symbol, Tree] = (params zip nParams).foldLeft(Map.empty){
             case (m, (oldParam, newParam)) => m.updated(oldParam.symbol, newParam)
         }
         changeSyms(association, body)

       private def changeIdent(body:Term, oldSym: Symbol, newSym: Symbol): Term =
         changeSyms(Map(oldSym->Ref(newSym)), body)

       private def changeSyms(association: Map[Symbol,Tree], body: Term): Term =
         val changes = new TreeMap() {

             def lookupParamTerm(symbol:Symbol): Option[Term] =
                association.get(symbol) match
                  case Some(paramTree) =>
                    paramTree match
                      case paramTerm: Term => Some(paramTerm)
                      case _ => throw MacroError(s"term expected for lambda param, we have ${paramTree}",posExprs(term))
                  case _ => None

             override def transformTerm(tree:Term)(using ctx: Context):Term =
               tree match
                 case ident@Ident(name) => lookupParamTerm(ident.symbol) match
                                             case Some(paramTerm) => paramTerm
                                             case None => super.transformTerm(tree)
                 case _ => super.transformTerm(tree)

             override def transformTypeTree(tree:TypeTree)(using ctx:Context):TypeTree =
               tree match
                 case Singleton(ref) =>
                           lookupParamTerm(ref.symbol) match
                               case Some(paramTerm) => Singleton(paramTerm)
                               case None => super.transformTypeTree(tree)
                 case a@Annotated(tp,annotation) =>
                           // bug in default TreeTransform, should process Annotated
                           Annotated.copy(a)(transformTypeTree(tp),transformTerm(annotation))
                 case i@Inferred() =>
                           Inferred(transformType(i.tpe))
                 case _ => super.transformTypeTree(tree)

             def transformType(tp: Type)(using ctx: Context): Type =
               tp match
                 case ConstantType(c) => tp
                 case tref@TermRef(qual, name) =>
                         lookupParamTerm(tref.termSymbol) match
                           case Some(paramTerm) => paramTerm.tpe
                           case None => tp
                 case tp: TypeRef =>
                         // it is impossible to create typeRef, so pass is itself
                         // TODO: add constructor to CompilerReflection
                         tp
                 case SuperType(thisTpe,superTpe) =>
                         SuperType(transformType(thisTpe),transformType(superTpe))
                 case Refinement(parent,name,info) =>
                         Refinement(transformType(parent),name,transformTypeOrBounds(info))
                 case AppliedType(tycon, args) =>
                         AppliedType(transformType(tycon), args.map(x => transformTypeOrBounds(x)))
                 case AnnotatedType(underlying, annot) =>
                         AnnotatedType(transformType(underlying), transformTerm(annot))
                 case AndType(rhs,lhs) => AndType(transformType(rhs),transformType(lhs))
                 case OrType(rhs,lhs) => OrType(transformType(rhs),transformType(lhs))
                 case MatchType(bound,scrutinee,cases) =>
                            MatchType(transformType(bound),transformType(scrutinee),
                                                        cases.map(x => transformType(x)))
                 case ByNameType(tp1) => ByNameType(transformType(tp1))
                 case ParamRef(x) => tp
                 case _ => tp  //  hope nobody will put termRef inside recursive type

             def transformTypeOrBounds(tpb: TypeOrBounds)(using ctx: Context): TypeOrBounds =
                   tpb match
                     case NoPrefix() => tpb
                     case TypeBounds(low,hi) => TypeBounds(transformType(low),transformType(hi))
                     case tp: Type => transformType(tp)


         }
         changes.transformTerm(body)

  }

  case class ApplyArgNamedRecord(term: NamedArg, name: String, nested: ApplyArgRecord )
     extends ApplyArgRecord {

       def index: Int = nested.index

       def hasShiftedLambda: Boolean = nested.hasShiftedLambda
       def isAsync: Boolean = nested.isAsync
       def noOrderDepended = nested.noOrderDepended
       def identArg: Term = NamedArg(name, nested.identArg)
       def shift(): ApplyArgRecord = copy(nested=nested.shift())
       def append(a: CpsTree): CpsTree = nested.append(a)

  }

  case class ApplyArgInlinedRecord(origin: Inlined, nested: ApplyArgRecord )
     extends ApplyArgRecord {
       def index: Int = nested.index
       def term: Term =
             Inlined(origin.call, origin.bindings, nested.term)
       def hasShiftedLambda: Boolean = nested.hasShiftedLambda
       def isAsync: Boolean = nested.isAsync
       def noOrderDepended = nested.noOrderDepended
       def identArg: Term = nested.identArg
       def shift(): ApplyArgRecord = copy(nested=nested.shift())
       def append(a: CpsTree): CpsTree =
             val na = nested.append(a)
             if (na eq a)
                a
             else
                InlinedCpsTree(origin, na)


  }


  case class ApplyArgByNameRecord(term: Term,
                                  index: Int,
                                  cpsTree: CpsTree,
                                  shifted: Boolean) extends ApplyArgRecord:
    def identArg: Term =
      if !shifted then
         term
      else
         val mt = MethodType(List())(_ => List(), _ => typeInMonad(term.tpe))
         Lambda(mt, args => cpsTree.transformed)

    def isAsync: Boolean = cpsTree.isAsync
    def hasShiftedLambda: Boolean = shifted
    def noOrderDepended: Boolean = true
    def shift() = copy(shifted = true)
    def append(tree: CpsTree): CpsTree = tree



  def termIsNoOrderDepended(x:Term): Boolean =
    x match {
      case Literal(_) => true
      case Ident(_) => if (x.symbol.isValDef) then
                         x.symbol.flags.is(Flags.Mutable)
                       else if x.symbol.isDefDef then
                         true
                       else if x.symbol.isBind then
                         true
                       else
                         false
      case _ => false
    }


