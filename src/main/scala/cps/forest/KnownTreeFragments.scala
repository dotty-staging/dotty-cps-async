package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait KnownTreeFragments[F[_], CT]:

  thisKnownTreeTransform: TreeTransformScope[F, CT] =>

  import qctx.reflect._

  lazy val awaitPure = '{ _root_.cps.await[F,Int](${cpsCtx.monad}.pure(3))(using ${cpsCtx.monad}) }.asReflectTree

  lazy val awaitSymbol = TransformUtil.find(awaitPure,
                           { case v@Select(x,m) if m == "await" => Some(v)
                             case v@Ident("await") => Some(v)
                             case _ => None
                           }).get.symbol

  lazy val monadTypeTree = TransformUtil.find(awaitPure,
                       { case TypeApply(Select(x,"await"),List(f1,f2)) => Some(f1)
                         case _ => None
                       }).get.asInstanceOf[TypeTree]


  lazy val pureSymbol = TransformUtil.find(awaitPure,
                           { case v@Select(x,m) if m == "pure" => Some(v)
                             case _ => None
                           }).get.symbol


  lazy val mapSymbol = {
        val mapTmpl =  '{ ${cpsCtx.monad}.map(${cpsCtx.monad}.pure(3))(_ + 1)  }
                         .asReflectTree

        TransformUtil.find(mapTmpl,
                           { case v@Select(x,m) if m == "map" => Some(v)
                             case _ => None
                           }).get.symbol
  }


  lazy val flatMapSymbol = {
        val flatMapTmpl =  '{ ${cpsCtx.monad}.flatMap(${cpsCtx.monad}.pure(3))( x =>
                                                                ${cpsCtx.monad}.pure(1 + x))  }.asReflectTree

        TransformUtil.find(flatMapTmpl,
                           { case v@Select(x,m) if m == "flatMap" => Some(v)
                             case _ => None
                           }).get.symbol
  }


  lazy val objAsyncShift = TypeIdent(Symbol.classSymbol("cps.ObjectAsyncShift")).tpe

  lazy val partialFunctionType = TypeIdent(Symbol.classSymbol("scala.PartialFunction")).tpe



