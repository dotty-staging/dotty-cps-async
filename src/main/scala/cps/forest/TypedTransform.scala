package cps.forest

import scala.quoted._

import cps._


class TypedTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  def run(using Quotes)(t: quotes.reflect.Term, tp: quotes.reflect.TypeTree): CpsExpr[F,T] =
     import quotes.reflect._
     val r = Async.nestTransform(t.asExprOf[T], cpsCtx, TransformationContextMarker.Typed)
     if (!r.isAsync)
       CpsExpr.sync(monad, patternCode)
     else
       // TODO:  create Typed with F[$tp] as type ?
       r



