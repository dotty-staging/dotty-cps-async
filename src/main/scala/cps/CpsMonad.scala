package cps

import scala.quoted._
import scala.util.Try
import scala.concurrent.duration._

trait CpsMonad[F[_]] {

   type WF[X] = F[X]

   def pure[T](t:T):F[T]

   def map[A,B](fa:F[A])(f: A=>B):F[B]

   def flatMap[A,B](fa:F[A])(f: A=>F[B]):F[B]

}



trait CpsTryMonad[F[_]] extends CpsMonad[F] {

   def error[A](e: Throwable): F[A]

   def restore[A](fa: F[A])(fx:Throwable => F[A]): F[A]

   def withAction[A](fa:F[A])(action: =>Unit):F[A] =
      flatMap(fa){x =>
        try{
          action
          pure(x)
        }catch{
          case ex: Throwable => error(ex)
        }
      }

}


trait CpsAsyncMonad[F[_]] extends CpsTryMonad[F] {

   /**
    * called by the source, which accept callback.
    * source is called immediatly in adoptCallbackStyle
    **/
   def adoptCallbackStyle[A](source: (Try[A]=>Unit) => Unit): F[A]

   def spawn[A](op: =>F[A]): F[A]

   def fulfill[T](t:F[T], timeout: Duration): Option[Try[T]]

}

object CpsMonad:

  given ForComprehensionSyntax as AnyRef:

    extension [F[_],T,S](x:F[T])(using m:CpsMonad[F])

      def flatMap(f: T=>F[S]): F[S] =
        m.flatMap(x)(f)

      def map(f: T=>S): F[S] =
        m.map(x)(f)

