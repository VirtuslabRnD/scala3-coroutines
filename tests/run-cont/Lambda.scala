import scala.continuations.*

@main def Test =
  println(indirectly(asInt(1.0) + asInt(2.0)))

def indirectly(comp: Int FailWith String): Either[String, Int] = FailingWith[String].run(comp)

def asInt(a: Double): Int FailWith String = if a.isWhole then a.toInt else fail("Not whole")
