Coroutines in Scala 3
=====
This repo is a fork of scala 3 repo: [original readme](https://github.com/lampepfl/dotty/blob/master/README.md)

This is an ongoing research project with the goal to test the limits of continuation-passing style programming in Scala 3 and check the feasibility of developing libraries that are using coroutines, generators, monadic reflection, and other similar constructs.

Status of the implementation
====
- [x] Possibility to define own executors
- [ ] Reentrant coroutine state - [#5](https://github.com/VirtuslabRnD/scala3-coroutines/issues/5)
- [ ] Calling one coroutine from the body of the other - [#8](https://github.com/VirtuslabRnD/scala3-coroutines/issues/8)
- [x] Inlining one coroutine in the body of the other
- [ ] Handling of exceptions thrown in coroutine body - [#16](https://github.com/VirtuslabRnD/scala3-coroutines/issues/16)
- [ ] Opaque coroutine state
- [ ] Composition of coroutine of different kind
#### Syntax elements working correctly inside of coroutines:
- [x] calls
- [x] assingments
- [x] block expressions
- [x] inlined calls
- [x] val definitions
- [ ] def definitions
- [ ] type definitions
- [ ] lazy val definitions
- [ ] loops - [#10](https://github.com/VirtuslabRnD/scala3-coroutines/issues/10)
- [ ] ifs - [#11](https://github.com/VirtuslabRnD/scala3-coroutines/issues/11)
- [ ] match expressions - [#12](https://github.com/VirtuslabRnD/scala3-coroutines/issues/12)
- [ ] try/catch - [#13](https://github.com/VirtuslabRnD/scala3-coroutines/issues/13)
- [ ] early return statements - [#14](https://github.com/VirtuslabRnD/scala3-coroutines/issues/14)
- [ ] throw statements - [#15](https://github.com/VirtuslabRnD/scala3-coroutines/issues/15)

How to try it
====
The project can be compiled and tested in the same way as dotty: [instructions here](https://dotty.epfl.ch/docs/contributing/getting-started.html). 

Examples of executors definitions and coroutines can be found in [the `continuations` directory of `run` tests](https://github.com/VirtuslabRnD/scala3-coroutines/tree/main/tests/run/continuations). Primitives are located in [the standard library](https://github.com/VirtuslabRnD/scala3-coroutines/blob/main/library/src/scala/continuations/primitives.scala) and they can be imported from anywhere. Executor definitions must be, on the other hand, pasted into the sources or repl.
