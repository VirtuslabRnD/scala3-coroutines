import scala.collection.*
import scala.continuations.*

@main def Test =
  println(FailingWith[String].run(asInt(1.0) + asInt(2.0)))
  println(FailingWith[String].run(asInt(1.0) + asInt(2.5)))
  println(FailingWith[String].run(asInt(1.5) + asInt(2.0)))
  println(FailingWith[String].run(asInt(1.5) + asInt(2.5)))

inline def asInt(a: Double): Int FailWith String = if a.isWhole then a.toInt else fail("Not whole")
