package cps.forest

import scala.quoted._

import cps._


class NewTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case Apply(fun,args)
  def run(using qctx: Quotes)(tp: qctx.reflect.TypeTree): CpsExpr[F,T] =
     CpsExpr.sync(monad, patternCode)




