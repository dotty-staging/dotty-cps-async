package cps

import scala.collection.ArrayOps
import scala.collection.IterableOps
import scala.collection.SeqOps
import scala.collection.MapOps
import scala.collection.immutable

trait AsyncShift[T]

trait AsyncShiftLowPriority1 {

 transparent inline given [A,CA <: Iterable[A] ] => AsyncShift[CA] as shiftedIterable =
      cps.runtime.IterableAsyncShift[A,CA]()

 transparent inline given [CA <: Range] => AsyncShift[CA] as shiftedRange =
        cps.runtime.RangeAsyncShift[CA]()

}

object AsyncShift extends AsyncShiftLowPriority1 {

 transparent inline given [A] => AsyncShift[scala.collection.ArrayOps[A]] as shiftedArrayOps =
      new cps.runtime.ArrayOpsAsyncShift[A]()

 transparent inline given [A,C[X] <: Iterable[X] & IterableOps[X,C,C[X]] ] => AsyncShift[C[A]] as shiftedIterableOps =
      new cps.runtime.IterableOpsAsyncShift[A,C,C[A]]()

 transparent inline given [A,C[X] <: Seq[X] & SeqOps[X,C,C[X]] ] => AsyncShift[C[A]] as shiftedSeqOps =
      cps.runtime.SeqAsyncShift[A,C,C[A]]()

 transparent inline given [K,V,CC[K,V] <: MapOps[K,V,CC,CC[K,V]] with Iterable[(K,V)]] => AsyncShift[CC[K,V]] as shiftedMapOps =
      cps.runtime.MapOpsAsyncShift[K,V,CC,Iterable,CC[K,V]]()

 transparent inline given [K,V,CC[K,V] <: MapOps[K,V,CC,CC[K,V]] with immutable.Iterable[(K,V)]] => AsyncShift[CC[K,V]] as shiftedImmutableMapOps =
      cps.runtime.MapOpsAsyncShift[K,V,CC,immutable.Iterable,CC[K,V]]()

 transparent inline given [A] => AsyncShift[scala.collection.immutable.List[A]] as shiftedList =
      cps.runtime.ListAsyncShift[A]()

 transparent inline   given $2 => (CpsMonad[F]) as AsyncShift[M] = new cps.runtime.CpsMonadSelfAsyncShift[F,M]

 transparent inline given [A] => AsyncShift[Option[A]] as shiftedOption =
      new cps.runtime.OptionAsyncShift[A]()

 transparent inline given [A,B] => AsyncShift[Function1[A,B]] as shiftedFunction1 =
      cps.runtime.Function1AsyncShift[A,B]()

 transparent inline given [A,B] => AsyncShift[PartialFunction[A,B]] as shiftedPartialFunction =
      cps.runtime.PartialFunctionAsyncShift[A,B]()

 transparent inline given [A] => AsyncShift[scala.util.Try[A]] as shiftedTry =
      new cps.runtime.util.TryAsyncShift[A]()

 transparent inline given shiftedUsing as AsyncShift[scala.util.Using.type] =
       cps.runtime.util.UsingAsyncShift

 transparent inline given [A,B] => AsyncShift[Either[A,B]] as shiftedEither =
      cps.runtime.util.EitherAsyncShift[A,B]()

 transparent inline given [A,B] => AsyncShift[Either.LeftProjection[A,B]] as shiftedEitherLeftProjection =
      cps.runtime.util.EitherLeftProjectionAsyncShift[A,B]()

}

trait AsyncShifted[T,F[_]]

trait ObjectAsyncShift[T]
{
  //def apply[F[_]](obj:T, cpsMonad: CpsMonad[F]): AsyncShifted[T,F]
}


