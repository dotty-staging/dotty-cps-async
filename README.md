# dotty-cps-async

## Description

This is the implementation of async/await transformation for Dotty (next version of the Scala programming language), based on an optimized version of cps[continuation passing style] transformation, where continuation is ‘pushed’ to the monad. 

Example:

```
   async[F:AsyncMonad]  {
        val connection = await(receiveConnection)
        try 
           while {
              val command = readComand(connection)
              val reply = await(handleOperation(command))
              Command != Shutdown
           } do ()
        finally 
           connection.close()
   }
```

Will be transformed to something like:

```
m.flatMap(openConnection())(a => {
  val connection: Connection[F] = a
  m.withAction({
    def _whilefun(): F[Unit] = 
       m.flatMap(
         m.flatMap(readCommand(connection))((a: Command) => {
          val command: Command = a
          m.flatMap(handle(command))((a: Reply) => {
            val reply: Reply = a
            m.flatMap(
             if (!reply.isMuted)
               connection.send(reply.toBytes) 
             else 
               m.pure(())
            )( _ => m.pure(!command.isShutdown))
      })
    }))(c => if (c) _whilefun() else m.pure(()))
    _whilefun()
  })(
    m.pure(connection.close())
  )
})
```

Note, that monad can be any trait, for which it is possible to implement [CpsAsyncMonad](https://github.com/rssh/dotty-cps-async/blob/master/src/main/scala/cps/CpsMonad.scala) typeclass. 
You can provide those methods for your favorite monad. Look at our implementation for 
[ComputationBounds](https://github.com/rssh/dotty-cps-async/blob/master/src/test/scala/cps/ComputationBound.scala) and [Future](https://github.com/rssh/dotty-cps-async/blob/master/src/main/scala/cps/FutureAsyncMonad.scala) for example.

Currently, doty-cps-async is at an early stage and not ready for production use.  Not all language constructions are handled. There are many things, which not work yet.   You can help to develop by providing test cases and implementing missing parts.  

## FAQ

* What are the current limitations?
    * all scala constructions are supported in async block. 
* Can we use await inside the argument of the high-order function? (like map or filter).
    * Yes, application developer or library author can provide ‘shifted’  implementation of the high-order function, which accepts  X=>M[Y] instead X=>Y.  This was implemented in the limited form in scala-gopher (paper: https://arxiv.org/abs/1611.00602)  for scala-2 and in practice, even in limited form, works quite well.
* I want to help with development. Where to start?
    * Open an issue (or select existing unassigned)  on GitHub and provide a preliminary plan for your work.  If you want to consult before choosing a task - ping me directly via e-mail or twitter.
* Is exists a version for scala-2?
    * No

## Presentations

### Can we free concurrent programming from the monadic style:
#### ScalaR:  https://www.youtube.com/watch?v=ImlUuTQUeaQ  (Jun 2020)
#### ScalaUA: https://www.youtube.com/watch?v=w-noRPLxYoA&t=3s  (Apr. 2020)
   (slides: https://www.slideshare.net/rssh1/can-concurrent-functional-programming-be-liberated-from-monadic-style )

## Related works

- Scala-continuations.  paper:  http://infoscience.epfl.ch/record/149136/files/icfp113-rompf.pdf
- Scala-async:   https://github.com/scala/scala-async  
- Storm-enroute coroutines:  https://drops.dagstuhl.de/opus/volltexte/2018/9208/pdf/LIPIcs-ECOOP-2018-3.pdf
- Thoughtworks DSL.scala:  https://github.com/ThoughtWorksInc/Dsl.scala
- Monadless.io: http://monadless.io/
- Effectfull: https://github.com/pelotom/effectful

   
