package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait TypeApplyTreeTransform[F[_], CT]:

  thisScope: TreeTransformScope[F, CT] =>

  import quotes.reflect._

  // case TypeApply(fun,targs)
  def runTypeApply( applyTerm: quotes.reflect.Term,
                    fun: quotes.reflect.Term,
                    targs: List[quotes.reflect.TypeTree]): CpsTree =
     runRoot(fun,TransformationContextMarker.TypeApplyFun).typeApply(targs, applyTerm.tpe)


object TypeApplyTreeTransform:


  def run[F[_]:Type,T:Type](using qctx1: Quotes)(cpsCtx1: TransformationContext[F,T],
                         applyTerm: qctx1.reflect.Term,
                         fun: qctx1.reflect.Term,
                         targs: List[qctx1.reflect.TypeTree]): CpsExpr[F,T] = {
     val tmpFType = Type[F]
     val tmpCTType = Type[T]
     class Bridge(tc:TransformationContext[F,T]) extends
                                                    TreeTransformScope[F,T]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType: Type[F] = tmpFType
         implicit val ctType: Type[T] = tmpCTType

         def bridge(): CpsExpr[F,T] =
            runTypeApply(applyTerm.asInstanceOf[quotes.reflect.Term],
                         fun.asInstanceOf[quotes.reflect.Term],
                         targs.asInstanceOf[List[quotes.reflect.TypeTree]]
                        ).toResult[T].asInstanceOf[CpsExpr[F,T]]

     }
     (new Bridge(cpsCtx1)).bridge()
  }

