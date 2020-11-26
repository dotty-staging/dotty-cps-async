package cps.features

import cps._

object implicitAwait:

  trait IsPossible[F[_]]

  inline given [F[_],T] => (CpsMonad[F], IsPossible[F]) => Conversion[F[T],T] as conversion =
           x => await(x)

