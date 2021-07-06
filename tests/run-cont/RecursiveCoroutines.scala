import scala.collection.*
import scala.continuations.*

@main def Test =
  val myEither = FailingWith[Double].run {
    val input: List[Double] = 2.0 :: 4.0 :: 6.0 :: 8.0 :: 10.0 :: 0.3 :: 1.0 :: Nil
    rec(input)
  }

  val myEither2 = FailingWith[Double].run {
    val input: List[Double] = 2.0 :: 4.0 :: 6.0 :: 8.0 :: 10.0 :: 0.0 :: 1.0 :: Nil
    rec(input)
  }

  println(myEither)
  println(myEither2)

def rec(list: List[Double]): List[Int] FailWith Double =
  if list == Nil then Nil
  else if list.head.isWhole then
    val tail = list.tail
    val recTail = rec(tail)
    list.head.toInt :: recTail
  else 
    val head = list.head
    fail(head)