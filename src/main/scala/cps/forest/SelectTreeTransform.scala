package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait SelectTreeTransform[F[_], CT]:

  thisScope: TreeTransformScope[F, CT] =>

  import qctx.reflect._

  // case selectTerm @ Select(qualifier,name)
  def runSelect( selectTerm: Select ): CpsTree =
     val symbol = selectTerm.symbol
     runRoot(selectTerm.qualifier, TransformationContextMarker.Select).applyTerm(_.select(symbol), selectTerm.tpe)


object SelectTreeTransform:


  def run[F[_]:Type,T:Type](using qctx1: Quotes)(cpsCtx1: TransformationContext[F,T],
                         selectTerm: qctx1.reflect.Select): CpsExpr[F,T] = {

     val tmpFType = Type[F]
     val tmpCTType = Type[T]
     class Bridge(tc:TransformationContext[F,T]) extends
                                                    TreeTransformScope[F,T]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType: Type[F] = tmpFType
         implicit val ctType: Type[T] = tmpCTType

         def bridge(): CpsExpr[F,T] =
            val origin = selectTerm.asInstanceOf[qctx.reflect.Select]
            runSelect(origin).toResult[T]


     }
     (new Bridge(cpsCtx1)).bridge()
  }

