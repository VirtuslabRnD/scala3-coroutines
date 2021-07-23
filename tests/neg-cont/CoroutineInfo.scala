import scala.continuations.*

trait Foo

def foo1(d: Double): Double FailWith String = ???
def foo2(using FailingWith[String]#C)(d: Double): Double = ??? // nopos-error
def foo3(d: Double)(using FailingWith[String]#C): Double = ???
def foo4(d: Double)(using Foo)(using FailingWith[String]#C): Double = ???
def foo5(d: Double)(using FailingWith[String]#C)(using Foo): Double = ??? // nopos-error
def foo6(d: Double)(using FailingWith[String]#C, Foo): Double = ??? // nopos-error
def foo7(d: Double)(using FailingWith[String]#C, FailingWith[Double]#C): Double = ??? // nopos-error
def foo8(d: Double)(using FailingWith[String]#C)(using FailingWith[Double]#C): Double = ??? // nopos-error
def foo9(d: Double)(using FailingWith[String]#C): Double FailWith Double = ??? // nopos-error
def foo10(d: Double): Double FailWith (String FailWith Double) = ??? //perfectly this should be an error but we don't catch this currently