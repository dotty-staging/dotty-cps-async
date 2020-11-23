package cps.forest

import scala.quoted._

import cps._


object TransformUtil:


  def find(using qctx:Quotes)(term: quotes.reflect.Term,
                       cond: quotes.reflect.Tree=> Option[quotes.reflect.Tree]) :Option[quotes.reflect.Tree] = {
     import quotes.reflect._
     import util._
     val search = new TreeAccumulator[Option[Tree]] {

        def foldTree(x: Option[Tree], tree: Tree)(using ctx: Context): Option[Tree] =
                 foldOverTree(x,tree)

        override def foldOverTree(x: Option[Tree], tree: Tree)(using ctx: Context): Option[Tree] = {
           if (x.isDefined)
             x
           else
             cond(tree) orElse super.foldOverTree(x,tree)
        }
     }
     search.foldTree(None,term)
  }

  def containsAwait(using qctx:Quotes)(term: quotes.reflect.Term): Boolean =
    import quotes.reflect._
    find(term, {
           case v@Apply(TypeApply(id@Ident("await"),targs),args) =>
                         if (id.symbol.fullName == "cps.await") Some(v) else None
           case _ => None
         }).isDefined


  /**
   * substitute identifier with the origin symbol to new tree
   **/
  def substituteIdent(using qctx:Quotes)(tree: quotes.reflect.Term,
                           origin: quotes.reflect.Symbol,
                           newTerm: quotes.reflect.Term): quotes.reflect.Term =
     import quotes.reflect._
     import util._
     val changes = new TreeMap() {
        override def transformTerm(tree:Term)(using ctx: Context):Term =
          tree match
            case ident@Ident(name) => if (ident.symbol == origin) {
                                         newTerm
                                      } else {
                                         super.transformTerm(tree)
                                      }
            case _ => super.transformTerm(tree)
     }
     changes.transformTerm(tree)


  def namedLet(using Quotes)(name: String, rhs: quotes.reflect.Term)(body: quotes.reflect.Ident => quotes.reflect.Term): quotes.reflect.Term = {
    import quotes.reflect._
    import scala.quoted.Quotes
    import scala.quoted.Expr
    // TODO use Block.let with name (see https://github.com/lampepfl/dotty/pull/10175)
    val expr = (rhs.asExpr: @unchecked) match {
      case '{ $rhsExpr: $t } =>
        '{
          // @showName(${Expr(name)})
          val x = $rhsExpr
          ${
            val id = Term.of('x).asInstanceOf[Ident]
            body(id).asExpr
          }
        }
    }
    Term.of(expr)
  }


