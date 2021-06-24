// extension (n: Int) inline def assertPositive: Int FailWith String =
//   if n > 0 then n else fail(s"$n is not positive")

// if n > 0 then n else fail(s"$n is not positive")


// inline def assertPositive(n: Int): Int FailWith Int =
//   //if n > 0 then n else fail("not positive")
//   if n > 0 then n else fail(n)

import scala.collection.*
import scala.continuations.*

inline def assertPositive(n: Int): Int FailWith String =
  if n > 0 then n else {
    val errorMsg = n.toString ++ " is not positive" //s"$n is not positive"
    fail(errorMsg)
  }

@main def Test =
  val three = FailingWith[String].run {
    val x: Int = assertPositive(3)
    x
  }
  val minusThree = FailingWith[String].run {
    val x: Int = assertPositive(-3)
    x
  }

  println(minusThree)
  // println(three)