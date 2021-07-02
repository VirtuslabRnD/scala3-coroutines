import scala.collection.*
import scala.continuations.*

// THIS TEST IS FAILING FOR NOW - INVESTIGATE WHY
// probable cause: lack of eta-expansion during lifting

@main def Test =
  val myEither = FailingWith[Double].run {
    val input: List[Double] = (2.0 :: 4.0 :: 6.0 :: 8.0 :: 10.0 :: 0.3 :: 1.0 :: Nil)
    println(rec(input))
  }

  val myEither2 = FailingWith[Double].run {
    val input: List[Double] = (2.0 :: 4.0 :: 6.0 :: 8.0 :: 10.0 :: 0.0 :: 1.0 :: Nil)
    println(rec(input))
  }

  println(myEither)
  println(myEither2)

def rec(list: List[Double]): List[Int] FailWith Double =
  if list == Nil then Nil else if list.head.isWhole then list.head.toInt :: rec(list.tail) else fail(list.head)
