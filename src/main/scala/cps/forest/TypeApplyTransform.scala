package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._


class TypeApplyTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case TypeApply(fun,targs)
  def run(using qctx: QuoteContext)(fun: qctx.tasty.Term, targs: List[qctx.tasty.TypeTree]): CpsExpr[F,T] =
     import qctx.tasty._
     TypeApplyTreeTransform.run(cpsCtx,patternCode.unseal, fun, targs)


