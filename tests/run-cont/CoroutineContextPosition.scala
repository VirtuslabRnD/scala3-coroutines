import scala.continuations.*

@main def Test =
  println(FailingWith[String].run {
    sqrt1(1)
  })
  println(FailingWith[String].run {
    sqrt2(4)
  })

trait Foo

def sqrt1(d: Double): Double FailWith String = if d >= 0 then Math.sqrt(d) else fail("negative")
def sqrt2(d: Double)(using FailingWith[String]#C): Double = if d >= 0 then Math.sqrt(d) else fail("negative")