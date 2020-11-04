package cps.forest

import scala.collection.immutable.Queue
import scala.quoted._
import cps._
import cps.misc._


trait CpsTreeScope[F[_], CT] {

  cpsTreeScope: TreeTransformScope[F, CT] =>

  import qctx.reflect._

  sealed abstract class CpsTree:

     def isAsync: Boolean

     def isSync: Boolean = ! isAsync

     def transformed: Term

     def syncOrigin: Option[Term]

     def typeApply(targs: List[qctx.reflect.TypeTree], ntpe: TypeRepr): CpsTree =
            applyTerm(_.appliedToTypeTrees(targs), ntpe)

     def applyTerm(f: Term => Term, ntpe: TypeReprTypeRepr): CpsTree

     def monadMap(f: Term => Term, ntpe:TypeRepr): CpsTree

     def monadFlatMap(f: Term => Term, ntpe:TypeRepr): CpsTree

     def append(next: CpsTree): CpsTree =
         // We should delay append resolving , to allow symbolic applying of await on sequence of appends
         AppendCpsTree(this, next)

     def appendFinal(next: CpsTree): CpsTree

     def prepend(prev: CpsTree): CpsTree =
          prev.append(this)

     def applyAwait(newOtpe: TypeRepr): CpsTree


     /**
      * type which is 'inside ' monad, i.e. T for F[T].
      **/
     def otpe: TypeRepr

     def toResult[T: Type] : CpsExpr[F,T] =
       import cpsCtx._

       def safeSeal(t:Term):Expr[Any] =
         t.tpe.widen match
           case MethodType(_,_,_) | PolyType(_,_,_) =>
             val ext = t.etaExpand
             ext.seal
           case _ => t.seal

       syncOrigin match
         case Some(syncTerm) =>
             CpsExpr.sync(monad,safeSeal(syncTerm).cast[T])
         case None =>
             val sealedTransformed = safeSeal(transformed).cast[F[T]]
             CpsExpr.async[F,T](monad, sealedTransformed)

     def toResultWithType[T](qt: Type[T]): CpsExpr[F,T] =
             given Type[T] = qt
             toResult[T]


  object CpsTree:

    def pure(origin:Term): CpsTree = PureCpsTree(origin)

    def impure(transformed:Term, tpe: TypeRepr): CpsTree =
                   AwaitSyncCpsTree(transformed, tpe.widen)

  case class PureCpsTree(origin: qctx.reflect.Term) extends CpsTree:

    def isAsync = false

    def typeApply(targs: List[qctx.reflect.TypeTree]) =
                PureCpsTree(origin.appliedToTypeTrees(targs))

    def applyTerm(x: Term => Term, ntpe: TypeRepr): CpsTree =
      PureCpsTree(x(origin))

     //  pure(x).map(f) = pure(f(x))
    def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
      PureCpsTree(f(origin))

    //   pure(x).flatMap(f:A=>M[B])
    def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
      FlatMappedCpsTree(this,f, ntpe)

    def appendFinal(next: CpsTree): CpsTree =
      next match
        case BlockCpsTree(statements,last) =>  //dott warn here.  TODO: research
             BlockCpsTree(statements.prepended(origin), last)
        case x: AsyncCpsTree =>
             BlockCpsTree(Queue(origin), x)
        case y: PureCpsTree =>
             BlockCpsTree(Queue(origin), y)
        //case _ =>
        //    BlockCpsTree(Queue(origin), next)


    def otpe: TypeRepr = origin.tpe.widen

    def syncOrigin: Option[Term] = Some(origin)

    def transformed: Term =
          val untpureTerm = cpsCtx.monad.unseal.select(pureSymbol)
          val tpureTerm = untpureTerm.appliedToType(otpe)
          val r = tpureTerm.appliedTo(origin)
          r

    def applyAwait(newOtpe: TypeRepr): CpsTree =
          AwaitSyncCpsTree(origin, newOtpe)


  abstract class AsyncCpsTree extends CpsTree:

    def isAsync = true

    def transformed: Term

    def syncOrigin: Option[Term] = None

    def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
          MappedCpsTree(this,f, ntpe)

    def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
          FlatMappedCpsTree(this,f, ntpe)

    def appendFinal(next: CpsTree): CpsTree =
          next match
            case syncNext: PureCpsTree => monadMap(_ => syncNext.origin, next.otpe)
            case asyncNext: AsyncCpsTree => monadFlatMap(_ => next.transformed, next.otpe)
            case blockNext: BlockCpsTree =>
                  blockNext.syncOrigin match
                    case Some(syncTerm) => monadMap(_ => syncTerm, next.otpe)
                    case None => monadFlatMap(_ => blockNext.transformed, next.otpe)

    def applyAwait(newOtpe: TypeRepr): CpsTree =
          AwaitAsyncCpsTree(this, newOtpe)


  case class AwaitSyncCpsTree(val origin: Term, val otpe: TypeRepr) extends AsyncCpsTree:

    def transformed: Term = origin

    def applyTerm(f: Term => Term, ntpe: TypeRepr): CpsTree =
          AwaitSyncCpsTree(f(transformed), ntpe)


  case class AwaitAsyncCpsTree(val nested: CpsTree, val otpe: TypeRepr) extends AsyncCpsTree:

    def transformed: Term =
      FlatMappedCpsTree(nested, (t:Term)=>t, otpe).transformed

    def applyTerm(f: Term => Term, ntpe: TypeRepr): CpsTree =
          AwaitAsyncCpsTree(nested.applyTerm(f,ntpe), ntpe)

    override def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
          FlatMappedCpsTree(nested, f, ntpe)

    override def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
          FlatMappedCpsTree(this, f, ntpe)


  case class MappedCpsTree(prev: CpsTree, op: Term => Term, otpe: TypeRepr) extends AsyncCpsTree:

    def applyTerm(f: Term => Term, npte: TypeRepr): CpsTree =
          MappedCpsTree(prev, t => f(op(t)), npte)

    override def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
          // this.map(f) = prev.map(op).map(f) = prev.map(op*f)
          // TODO: rethink. Mb add val if t have multiple entries in f
          MappedCpsTree(prev, t => f(op(t)), ntpe)
          //  disabled due to https://github.com/lampepfl/dotty/issues/9254
          //MappedCpsTree(this, t=>f(t) , ntpe)

    override def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
          // this.flatMap(f) = prev.map(op).flatMap(f) = prev.flatMap(op*f)
          // FlatMappedCpsTree(prev, t => f(op(t)), ntpe)
          //  disabled due to https://github.com/lampepfl/dotty/issues/9254
          FlatMappedCpsTree(this, f, ntpe)

    def transformed: Term = {
          val untmapTerm = cpsCtx.monad.unseal.select(mapSymbol)
          val wPrevOtpe = prev.otpe.widen
          val wOtpe = otpe.widen
          val tmapTerm = untmapTerm.appliedToTypes(List(wPrevOtpe,wOtpe))
          val r = tmapTerm.appliedToArgss(
                     List(List(prev.transformed),
                          List(
                            Lambda(
                              MethodType(List("x"))(mt => List(wPrevOtpe), mt => wOtpe),
                              opArgs => op(opArgs.head.asInstanceOf[Term])
                            )
                          )
                     )
          )
          //val r = '{
          //   ${cpsCtx.monad}.map(${prev.transformed.seal.asInstanceOf[F[T]]})(
          //             (x:${prev.seal}) => ${op('x)}
          //   )
          //}.unseal
          r
    }

    override def applyAwait(newOtpe: TypeRepr): CpsTree =
       FlatMappedCpsTree(prev, op, newOtpe)



  case class FlatMappedCpsTree(
                      val prev: CpsTree,
                      opm: Term => Term,
                      otpe: TypeRepr) extends AsyncCpsTree:

    def applyTerm(f: Term => Term, npte: TypeRepr): CpsTree =
          FlatMappedCpsTree(prev, t => f(opm(t)), npte)

    override def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
          // this.map(f) = prev.flatMap(opm).map(f) = prev.flr(opm*f)
          FlatMappedCpsTree(prev, t => f(opm(t)), ntpe)

    override def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
          // this.flatMap(f) = prev.flatMap(opm).flatMap(f)
          FlatMappedCpsTree(this,f,ntpe)

    def transformed: Term = {
        // ${cpsCtx.monad}.flatMap(${prev.transformed})((x:${prev.it}) => ${op('x)})
        val monad = cpsCtx.monad.unseal
        val untpFlatMapTerm = monad.select(flatMapSymbol)
        val wPrevOtpe = prev.otpe.widen
        val wOtpe = otpe.widen
        val tpFlatMapTerm = untpFlatMapTerm.appliedToTypes(List(wPrevOtpe,wOtpe))
        val r = tpFlatMapTerm.appliedToArgss(
            List(
              List(prev.transformed),
              List(
                Lambda(
                  MethodType(List("x"))(mt => List(wPrevOtpe),
                                        mt => fType.unseal.tpe.appliedTo(wOtpe)),
                  opArgs => opm(opArgs.head.asInstanceOf[Term])
                )
             )
           )
        )
        r
    }


  end FlatMappedCpsTree

  case class BlockCpsTree(prevs:Queue[Statement], last:CpsTree) extends CpsTree:

    override def isAsync = last.isAsync

    def toLast(f:CpsTree=>CpsTree):CpsTree =
      if (prevs.isEmpty)
        f(last)
      else
        BlockCpsTree(prevs,f(last))

    override def transformed: Term =
      if (prevs.isEmpty)
        last.transformed
      else
        Block(prevs.toList, last.transformed)

    override def syncOrigin: Option[Term] =
      if prevs.isEmpty then
        last.syncOrigin
      else
        last.syncOrigin map (l => Block(prevs.toList,l))

    def applyTerm(f: Term => Term, ntpe: TypeRepr): CpsTree =
       toLast(_.applyTerm(f,ntpe))

    def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
       toLast(_.monadMap(f,ntpe))

    def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
       toLast(_.monadFlatMap(f,ntpe))

    def appendFinal(next: CpsTree): CpsTree =
       last.syncOrigin match
         case Some(syncLast) => BlockCpsTree(prevs.appended(syncLast),next)
         case None => BlockCpsTree(prevs, last.appendFinal(next))

    def otpe: TypeRepr = last.otpe

    override def applyAwait(newOtpe: TypeRepr): CpsTree =
        BlockCpsTree(prevs, last.applyAwait(newOtpe))

  end BlockCpsTree

  case class InlinedCpsTree(origin: Inlined, nested: CpsTree) extends CpsTree:

    override def isAsync = nested.isAsync

    override def transformed: Term =
                  Inlined(origin.call, origin.bindings, nested.transformed)

    override def syncOrigin: Option[Term] =
                  nested.syncOrigin.map(Inlined(origin.call, origin.bindings, _ ))

    def applyTerm(f: Term => Term, ntpe: TypeRepr): CpsTree =
         InlinedCpsTree(origin, nested.applyTerm(f, ntpe))

    def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
         InlinedCpsTree(origin, nested.monadMap(f, ntpe))

    def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
         InlinedCpsTree(origin, nested.monadFlatMap(f, ntpe))

    def appendFinal(next: CpsTree): CpsTree =
         InlinedCpsTree(origin, nested.appendFinal(next))

    def otpe: TypeRepr = nested.otpe

    override def applyAwait(newOtpe:TypeRepr): CpsTree =
         InlinedCpsTree(origin, nested.applyAwait(newOtpe))

  end InlinedCpsTree

  case class ValCpsTree(valDef: ValDef, rightPart: CpsTree, nested: CpsTree) extends CpsTree:

    override def isAsync = rightPart.isAsync || nested.isAsync

    override def transformed: Term =
       rightPart.syncOrigin match
         case Some(rhs) =>
           appendValDef(rhs)
         case None =>
           if (nested.isAsync)
              rightPart.monadFlatMap(v => appendValDef(v) , nested.otpe).transformed
           else
              rightPart.monadMap(v => appendValDef(v) , nested.otpe).transformed

    override def syncOrigin: Option[Term] =
       for{
           rhs <- rightPart.syncOrigin
           next <- nested.syncOrigin
       } yield appendValDef(rhs)

    override def applyTerm(f: Term => Term, ntpe: TypeRepr): CpsTree =
        ValCpsTree(valDef, rightPart, nested.applyTerm(f,ntpe))

    override def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
        ValCpsTree(valDef, rightPart, nested.monadMap(f,ntpe))

    override def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
        ValCpsTree(valDef, rightPart, nested.monadFlatMap(f,ntpe))

    override def appendFinal(next: CpsTree): CpsTree =
        ValCpsTree(valDef, rightPart, nested.appendFinal(next))

    override def otpe: TypeRepr = nested.otpe

    override def applyAwait(newOtpe: TypeRepr): CpsTree =
        ValCpsTree(valDef, rightPart, nested.applyAwait(newOtpe))

    def appendValDef(right: Term):Term =
       val nValDef = ValDef.copy(valDef)(name = valDef.name, tpt=valDef.tpt, rhs=Some(right))
       val result = nested match
         case BlockCpsTree( prevs,last) =>
           val lastTerm = last.syncOrigin.getOrElse(last.transformed)
           Block(nValDef +: prevs.toList, lastTerm)
         case _ =>
           val next = nested.syncOrigin.getOrElse(nested.transformed)
           appendValDefToNextTerm(nValDef, next)
       result

    def appendValDefToNextTerm(valDef: ValDef, next:Term): Term =
       next match
         case x@Lambda(params,term) => Block(List(valDef), x)
         case Block(stats, last) => Block(valDef::stats, last)
         case other => Block(List(valDef), other)


  end ValCpsTree

  /**
   * append cps tree, which is frs and then snd.
   * we use this representation instead Mapped/Flatmapped in cases,
   * where we later can apply await to append term and simplify tree
   * instead wrapping awaited tree in extra flatMap
   */
  case class AppendCpsTree(frs: CpsTree, snd: CpsTree) extends CpsTree:

    def isAsync = frs.isAsync || snd.isAsync

    override def transformed: Term =
         frs.appendFinal(snd).transformed

    override def syncOrigin: Option[Term] = {
       // TODO: insert warning about discarded values
       for{ x <- frs.syncOrigin
            y <- snd.syncOrigin
          } yield {
            x match
              case Block(xStats, xLast) =>
                y match
                  case Block(yStats, yLast) =>
                    Block((xStats :+ xLast) ++ yStats, yLast)
                  case yOther =>
                    Block(xStats :+ xLast, yOther)
              case xOther =>
                y match
                  case Block(yStats, yLast) =>
                    Block(xOther::yStats, yLast)
                  case yOther =>
                    Block(xOther::Nil, yOther)
          }
    }

    def applyTerm(x: Term => Term, ntpe: TypeRepr): CpsTree =
         AppendCpsTree(frs, snd.applyTerm(x, ntpe))

    override def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
         AppendCpsTree(frs, snd.monadMap(f, ntpe))

    override def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
         AppendCpsTree(frs, snd.monadFlatMap(f, ntpe))

    override def appendFinal(next: CpsTree): CpsTree =
         frs.appendFinal(snd.appendFinal(next))

    override def applyAwait(newOtpe: TypeRepr): CpsTree =
         // TODO: insert optimization
         AppendCpsTree(frs, snd.applyAwait(newOtpe))

    override def otpe: TypeRepr = snd.otpe

  end AppendCpsTree


}
