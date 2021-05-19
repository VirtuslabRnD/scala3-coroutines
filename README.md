Coroutines in Scala 3
=====
This repo is a fork of scala 3 repo: [original readme](https://github.com/lampepfl/dotty/blob/master/README.md)

This is an ongoing research project with the goal to test the limits of continuation-passing style programming in Scala 3 and check the feasibility of developing libraries that are using coroutines, generators, monadic reflection, and other similar constructs.

Status of the implementation
====
- [x] Possibility to define own executors
- [ ] Reentrant coroutine state
- [ ] Calling one coroutine from the body of the other
- [x] Inlining one coroutine in the body of the other
- [ ] Handling of exceptions thrown in coroutine body
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
- [ ] loops
- [ ] ifs
- [ ] match expressions
- [ ] try/catch
- [ ] early return statements
- [ ] throw statements

How to try it
====
TBD
