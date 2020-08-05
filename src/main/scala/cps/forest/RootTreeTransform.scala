package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait RootTreeTransform[F[_], CT]:

  thisTransform: TreeTransformScope[F, CT] =>

  import qctx.tasty._

  def runRoot(term: qctx.tasty.Term): CpsTree =
     if (cpsCtx.flags.debugLevel >= 15)
        cpsCtx.log(s"runRoot: term=$safeShow(term)")
     val r = term.tpe.widen match {
       case _ : MethodType =>
               //  in such case, we can't transform tree to expr
               //  without eta-expansion.
               //    from other side - we don't want do eta-expand now, it can be performed early.
                runRootUneta(term)
       case _ : PolyType =>
                runRootUneta(term)
       case _ =>
                val expr = term.asExpr
                val monad = cpsCtx.monad
                expr match {
                  case '{ $e: $et } =>
                     val rCpsExpr = Async.nestTransform(e, cpsCtx, "_")
                     val r = exprToTree(rCpsExpr, term)
                     if (cpsCtx.flags.debugLevel >= 10)
                         cpsCtx.log(s"rCpsExpr=$rCpsExpr, async=${rCpsExpr.isAsync}")
                         cpsCtx.log(s"r=$r, async=${r.isAsync}")
                     r
                  case _ =>
                     throw MacroError("Can't determinate exact type for term", expr)
                }
     }
     if (cpsCtx.flags.debugLevel >= 15)
        cpsCtx.log(s"runRoot result: $r")
     r


  def runRootUneta(term: qctx.tasty.Term): CpsTree = {
     if (cpsCtx.flags.debugLevel >= 15)
        cpsCtx.log(s"runRootUneta, term=$term")
     val monad = cpsCtx.monad
     val r = term match {
       case Select(qual, name) =>
           runRoot(qual) match
              case rq: AsyncCpsTree =>
                  val cTransformed = rq.transformed.asInstanceOf[qctx.tasty.Term]
                  CpsTree.impure(Select(cTransformed,term.symbol),term.tpe)
              case _: PureCpsTree =>
                  CpsTree.pure(term)
       case Ident(name) =>
             CpsTree.pure(term)
       case Apply(x, args) =>
             runApply(term,x,args,Nil)
       case _ =>
             throw MacroError(s"cps tree transform is not supported yet to ${term}",cpsCtx.patternCode)
     }
     if (cpsCtx.flags.debugLevel >= 15)
        cpsCtx.log(s"runRootUneta result: $r  (term=$term)")
     r
  }

  def exprToTree(expr: CpsExpr[F,_], e: Term): CpsTree =
     if (expr.isAsync)
         val transformed = expr.transformed.asTerm
         AwaitCpsTree(transformed, e.tpe)
     else
         PureCpsTree(e)



