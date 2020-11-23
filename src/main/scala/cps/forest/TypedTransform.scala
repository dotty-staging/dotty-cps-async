package cps.forest

import scala.quoted._

import cps._


class TypedTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  def run(using Quotes)(t: qctx.reflect.Term, tp: qctx.reflect.TypeTree): CpsExpr[F,T] =
     import qctx.reflect._
     val r = Async.nestTransform(t.asExprOf[T], cpsCtx, TransformationContextMarker.Typed)
     if (!r.isAsync)
       CpsExpr.sync(monad, patternCode)
     else
       // TODO:  create Typed with F[$tp] as type ?
       r



