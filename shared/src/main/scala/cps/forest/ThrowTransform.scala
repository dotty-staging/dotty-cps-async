package cps.forest

import scala.quoted._

import cps._
import cps.misc._

object ThrowTransform:

  /**
   *'''
   * '{ throw $ex }
   *'''
   **/
  def run[F[_]:Type,T:Type, S<:Throwable:Type](cpsCtx: TransformationContext[F,T],
                               ex: Expr[S]
                               )(using Quotes): CpsExpr[F,T] =
     import quotes.reflect._
     import util._
     import cpsCtx._
     val cpsEx = Async.nestTransform(ex, cpsCtx, TransformationContextMarker.ThrowException)

     if (cpsCtx.monad.asTerm.tpe <:< TypeRepr.of[CpsTryMonad[F]])
       val errorMonad = monad.asExprOf[CpsTryMonad[F]]
       if (!cpsEx.isAsync)
            // TODO: think, mb leave as is...
            CpsExpr.async[F,T](monad,  '{  ${errorMonad}.error(${ex}) })
       else
            CpsExpr.async[F,T](monad,
                cpsEx.flatMap[T]( '{ (ex:S) => ${errorMonad}.error(ex) } ).transformed )
     else
       throw MacroError("this monad not support try/catch",patternCode)

