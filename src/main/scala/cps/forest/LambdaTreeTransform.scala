package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait LambdaTreeTransform[F[_], CT]:

  thisScope: TreeTransformScope[F, CT] =>

  import quotes.reflect._

  def typeInMonad(tp:TypeRepr): TypeRepr =
       Term.of(fType).tpe.appliedTo(tp)

  // case lambdaTree @ Lambda(params,body)
  def runLambda(lambdaTerm: Term, params: List[ValDef], expr: Term ): CpsTree =
     if (cpsCtx.flags.debugLevel >= 10)
       cpsCtx.log(s"runLambda, lambda=${safeShow(lambdaTerm)}")
       cpsCtx.log(s"runLambda, expr=${safeShow(expr)}")
     val cpsBody = runRoot(expr,TransformationContextMarker.Lambda)
     val retval = if (cpsBody.isAsync) {
        // in general, shifted lambda
        if (cpsCtx.flags.allowShiftedLambda) then
            asyncBodyShiftedLambda(lambdaTerm, params, cpsBody)
        else
            throw MacroError("await inside lambda functions without enclosing async block", lambdaTerm.asExpr)
     } else {
        CpsTree.pure(lambdaTerm)
     }
     retval

  def asyncBodyShiftedLambda(lambdaTerm: Term, params: List[ValDef], cpsBody: CpsTree): CpsTree =
     val paramNames = params.map(_.name)
     val paramTypes = params.map(_.tpt.tpe)
     val shiftedType = shiftedMethodType(paramNames, paramTypes, cpsBody.otpe)
     // TODO: think, maybe exists case, where we need substitute Ident(param) for x[i] (?)
     //       because otherwise it's quite strange why we have such interface in compiler
     val rLambda = Lambda(shiftedType, (x: List[Tree]) => cpsBody.transformed )
     CpsTree.pure(rLambda)


  def shiftedMethodType(paramNames: List[String], paramTypes:List[TypeRepr], otpe: TypeRepr): MethodType =
     MethodType(paramNames)(_ => paramTypes, _ => typeInMonad(otpe))




object LambdaTreeTransform:


  def run[F[_]:Type,T:Type](using qctx1: Quotes)(cpsCtx1: TransformationContext[F,T],
                         lambdaTerm: qctx1.reflect.Term,
                         params: List[qctx1.reflect.ValDef],
                         expr: qctx1.reflect.Term): CpsExpr[F,T] = {

     val tmpFType = Type[F]
     val tmpCTType = Type[T]
     class Bridge(tc:TransformationContext[F,T]) extends
                                                    TreeTransformScope[F,T]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType: Type[F] = tmpFType
         implicit val ctType: Type[T] = tmpCTType

         def bridge(): CpsExpr[F,T] =
            val origin = lambdaTerm.asInstanceOf[quotes.reflect.Term]
            val xparams = params.asInstanceOf[List[quotes.reflect.ValDef]]
            val xexpr   = expr.asInstanceOf[quotes.reflect.Term]
            runLambda(origin, xparams, xexpr).toResult[T]


     }
     (new Bridge(cpsCtx1)).bridge()
  }


