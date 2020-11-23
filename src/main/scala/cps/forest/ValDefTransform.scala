package cps.forest

import scala.quoted._

import cps._
import cps.misc._


object ValDefTransform:


  def fromBlock[F[_]:Type](using qctx:Quotes)(
                           cpsCtx: TransformationContext[F,Unit],
                           valDef: quotes.reflect.ValDef): CpsExpr[F,Unit] = {
     import quotes.reflect._
     import cpsCtx._
     if (cpsCtx.flags.debugLevel >= 15)
       cpsCtx.log(s"ValDefExpr:fromBlock, valDef=$valDef")
     val rhs = valDef.rhs.getOrElse(
             throw MacroError(s"val $valDef without right part in block ", cpsCtx.patternCode)
     )
     rhs.asExpr match
        case '{ $e: $et } =>
            if (cpsCtx.flags.debugLevel > 15)
               cpsCtx.log(s"rightPart is ${e.show}")
            val cpsRight = Async.nestTransform(e,cpsCtx,TransformationContextMarker.ValDefRight)
            if (cpsRight.isAsync) {
               if (cpsCtx.flags.debugLevel > 15) {
                  cpsCtx.log(s"rightPart is async")
               }
               RhsFlatMappedCpsExpr(using qctx)(monad, Seq(),
                                                valDef, cpsRight, CpsExpr.unit(monad) )
            } else {
               if (cpsCtx.flags.debugLevel > 15)
                 cpsCtx.log(s"rightPart is no async, cpsRight.transformed=${cpsRight.transformed.show}")
               ValWrappedCpsExpr(using qctx)(monad, Seq(), valDef,
                                                CpsExpr.unit(monad) )
            }
        case other =>
            throw MacroError(s"Can't concretize type of right-part $rhs ", cpsCtx.patternCode)


  }


  class RhsFlatMappedCpsExpr[F[_]:Type, T:Type, V:Type](using qctx:Quotes)
                                     (monad: Expr[CpsMonad[F]],
                                      prev: Seq[ExprTreeGen],
                                      oldValDef: quotes.reflect.ValDef,
                                      cpsRhs: CpsExpr[F,V],
                                      next: CpsExpr[F,T]
                                     )
                                    extends AsyncCpsExpr[F,T](monad, prev) {

       override def fLast(using Quotes) =
          import quotes.reflect._

          def appendBlockExpr[A:Type](rhs: quotes.reflect.Term, expr: Expr[A]):Expr[A] =
                buildAppendBlockExpr(oldValDef.asInstanceOf[quotes.reflect.ValDef],
                                     rhs, expr)

          next.syncOrigin match
            case Some(nextOrigin) =>
             '{
               ${monad}.map(${cpsRhs.transformed})((v:V) =>
                          ${appendBlockExpr('Term.of(v), nextOrigin)})
              }
            case  None =>
             '{
               ${monad}.flatMap(${cpsRhs.transformed})((v:V)=>
                          ${appendBlockExpr(Term.of('v), next.transformed)})
             }

       override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T] =
          if (exprs.isEmpty)
             this
          else
             RhsFlatMappedCpsExpr(using qctx)(monad, exprs ++: prev,oldValDef,cpsRhs,next)


       override def append[A:Type](e: CpsExpr[F,A])(using qtcx: Quotes) =
          RhsFlatMappedCpsExpr(using qctx)(monad,prev,oldValDef,cpsRhs,next.append(e))


       private def buildAppendBlock(using qctx:Quotes)(
                      oldValDef: quotes.reflect.ValDef, rhs: quotes.reflect.Term,
                                                    exprTerm:quotes.reflect.Term): quotes.reflect.Term =
       {
          import quotes.reflect._
          import scala.quoted.Quotes
          import scala.quoted.Expr

          val valDef = ValDef(oldValDef.symbol, Some(rhs)).asInstanceOf[quotes.reflect.ValDef]
          exprTerm match
              case Block(stats,last) =>
                    Block(valDef::stats, last)
              case other =>
                    Block(valDef::Nil,other)

       }

       private def buildAppendBlockExpr[A:Type](using Quotes)(oldValDef: quotes.reflect.ValDef, rhs: quotes.reflect.Term, expr:Expr[A]):Expr[A] =
          import quotes.reflect._
          buildAppendBlock(oldValDef,rhs,Term.of(expr)).asExprOf[A]

  }

  class ValWrappedCpsExpr[F[_]:Type, T:Type, V:Type](using Quotes)(
                                      monad: Expr[CpsMonad[F]],
                                      prev: Seq[ExprTreeGen],
                                      oldValDef: quotes.reflect.ValDef,
                                      next: CpsExpr[F,T] ) extends AsyncCpsExpr[F,T](monad,prev):


       override def isAsync = next.isAsync

       override def syncOrigin(using qctx:Quotes): Option[Expr[T]] = next.syncOrigin.map{ n =>
         import quotes.reflect._
         val prevStats: List[Statement] = prev.map(_.extract).toList
         val valDef: Statement = oldValDef.asInstanceOf[quotes.reflect.ValDef]
         val outputTerm = Term.of(n) match
            case Block(statements, last) =>
                   Block( prevStats ++: (valDef +: statements), last)
            case other =>
                   Block( prevStats ++: List(valDef), other)
         outputTerm.asExprOf[T]
       }


       override def fLast(using Quotes) = next.fLast

       override def transformed(using Quotes) = {
          import quotes.reflect._

          val valDef = oldValDef.asInstanceOf[quotes.reflect.ValDef]
          val block = Term.of(next.transformed) match
             case Block(stats, e) =>
                 Block( prev.map(_.extract) ++: valDef +: stats, e)
             case other =>
                 Block( prev.map(_.extract) ++: List(valDef) , other)
          block.asExprOf[F[T]]

       }

       override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T] =
          if (exprs.isEmpty)
            this
          else
            ValWrappedCpsExpr[F,T,V](using qctx)(monad, exprs ++: prev, oldValDef, next)

       override def append[A:Type](e:CpsExpr[F,A])(using Quotes) =
           ValWrappedCpsExpr(using qctx)(monad, prev,
                                         oldValDef.asInstanceOf[quotes.reflect.ValDef],
                                         next.append(e))


       def prependPrev(using qctx:Quotes)(term: quotes.reflect.Term): quotes.reflect.Term =
          import quotes.reflect._
          if (prev.isEmpty) {
             term
          } else {
             term match
               case Block(stats, expr) =>
                 Block(prev.map(_.extract) ++: stats, expr)
               case other =>
                 Block(prev.toList.map(_.extract) , other)
          }

