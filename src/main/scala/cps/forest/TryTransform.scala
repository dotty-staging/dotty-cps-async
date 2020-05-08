package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


class TryTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case Try(body, cases, finalizer)
  def run(using qctx: QuoteContext)(body: qctx.tasty.Term,
                                    cases: List[qctx.tasty.CaseDef],
                                    finalizer: Option[qctx.tasty.Term]): CpsExpr[F,T] =
     import qctx.tasty._
     val cpsBody = Async.nestTransform(body.seal.asInstanceOf[Expr[T]],
                                            cpsCtx, "B")
     val cpsCaseDefs = cases.zipWithIndex.map((cd,i) => Async.nestTransform(
                                                  cd.rhs.seal.asInstanceOf[Expr[T]],
                                                  cpsCtx, i.toString))
     val isCaseDefsAsync = cpsCaseDefs.exists(_.isAsync)
     val optCpsFinalizer = finalizer.map( x => Async.nestTransform[F,T,Unit](
                                        x.seal.asInstanceOf[Expr[Unit]], cpsCtx, "F"))
     val isFinalizerAsync = optCpsFinalizer.exists(_.isAsync)
     val isAsync = cpsBody.isAsync || isCaseDefsAsync || isFinalizerAsync

     def makeRestoreExpr(): Expr[Throwable => F[T]]  =
        val nCaseDefs = (cases lazyZip cpsCaseDefs) map { (frs,snd) =>
           CaseDef(frs.pattern, frs.guard, snd.transformed.unseal)
        }
        val restoreExpr = '{ (ex: Throwable) => ${Match('ex.unseal, nCaseDefs.toList).seal} }
        restoreExpr.asInstanceOf[Expr[Throwable => F[T]]]



     val builder = if (!isAsync) {
                      CpsExpr.sync(monad, patternCode)
                   } else {
                      val errorMonad = if (monad.unseal.tpe <:< '[CpsTryMonad[F]].unseal.tpe) {
                                          monad.asInstanceOf[Expr[CpsTryMonad[F]]]
                                      } else {
                                          throw MacroError(s"${monad} should be instance of CpsTryMonad for try/catch support", patternCode)
                                      }
                      optCpsFinalizer match
                        case None =>
                           if (cpsCaseDefs.isEmpty)
                             cpsBody
                           else
                             CpsExpr.async[F,T](cpsCtx.monad,
                               '{
                                 ${errorMonad}.restore(
                                   ${cpsBody.transformed}
                                   )(${makeRestoreExpr()})
                               })
                        case Some(cpsFinalizer) =>
                           if (cpsCaseDefs.isEmpty)
                             CpsExpr.async[F,T](cpsCtx.monad,
                               '{
                                  ${errorMonad}.withAction(
                                    ${cpsBody.transformed}
                                  )(${cpsFinalizer.transformed})
                               })
                           else
                             CpsExpr.async[F,T](cpsCtx.monad,
                                 '{
                                   ${errorMonad}.withAction(
                                    ${errorMonad}.restore(
                                     ${cpsBody.transformed}
                                    )(${makeRestoreExpr()})
                                   )(${cpsFinalizer.transformed})
                                 })
                   }
     builder


