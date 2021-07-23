import scala.continuations.*

class Foo:
  override def toString = "Foo"
class Bar(i: Int):
  override def toString = s"Bar(${i})"

@main def Test =
  println(FailingWith[String].run {
    new Foo
  })
  println(FailingWith[String].run {
    new Bar(10)
  })
  println(FailingWith[String].run {
    new Bar(fail("Cannot create new Bar"))
  })
