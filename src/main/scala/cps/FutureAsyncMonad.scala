package cps

import scala.concurrent._
import scala.concurrent.duration._
import scala.quoted._
import scala.util._

given FutureAsyncMonad(using ExecutionContext) as CpsAsyncMonad[Future]: 

   type F[+T] = Future[T]

   override type WF[T] = F[T]

   def pure[T](t:T):Future[T] = Future(t)

   def map[A,B](fa:F[A])(f: A=>B):F[B] =
        fa.map(f)

   def flatMap[A,B](fa:F[A])(f: A=>F[B]):F[B] =
        fa.flatMap(f)

   def error[A](e: Throwable): F[A] =
        Future.failed(e)
   
   def restore[A](fa: F[A])(fx:Throwable => F[A]): F[A] =
        fa.recoverWith{ case ex => fx(ex) }
   
   def adoptCallbackStyle[A](source: (Try[A]=>Unit) => Unit): F[A] =
        val p = Promise[A]
        source(p.complete(_))
        p.future

   def spawn[A](op: => F[A]): F[A] = 
        val p = Promise[A]
        summon[ExecutionContext].execute( () => p.completeWith(op) )
        p.future
         

   def fulfill[T](t:F[T], timeout: Duration): Option[Try[T]] =
        try
          Await.ready(t, timeout)
          t.value
        catch
          case ex: TimeoutException => t.value

   def executionContext = summon[ExecutionContext]

given implicitAwait.IsPossible[Future]

given fromFutureConversion[G[_]](using ExecutionContext, CpsAsyncMonad[G]) as CpsMonadConversion[Future,G] =
   new CpsMonadConversion[Future, G] {
     override def apply[T](mf: CpsMonad[Future], mg: CpsMonad[G], ft:Future[T]): G[T] =
           summon[CpsAsyncMonad[G]].adoptCallbackStyle(
                                         listener => ft.onComplete(listener) )
   }
