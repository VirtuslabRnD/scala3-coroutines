import scala.collection.*
import scala.continuations.*

class A:
  class B

@main def Test =
  val myEither = FailingWith[String].run {
    val d: Double = 2.5 * fallible1.extract
    val r: Int = asInt(d)
    println("I'm here?")
    r + 5
  }

  val myEither2 = FailingWith[String].run {
    val d: Double = 10 * fallible1.extract
    // val r: Int = asInt(d)
    val r: Int = if d.isWhole then d.toInt else fail("Not whole")
    println("I'm here!")
    println("And r = " + r)
    r + 5
  }

  println(myEither)
  println(myEither2)

def fallible1: Either[String, Double] = Right(1.0)
inline def asInt(a: Double): Int FailWith String = if a.isWhole then a.toInt else fail("Not whole")