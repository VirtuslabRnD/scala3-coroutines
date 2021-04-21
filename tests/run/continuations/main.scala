import scala.collection.*
import scala.continuations.*

@main def Test =
  val mySeq = Lazily[Int].run {
    give(2+4)
    for x <- 7 to 9 do give(x)
    give(10 / 0)
  }
  assert(mySeq.iterator.take(3).toList == List(6, 7, 8))

  val myEither = WithEither[String, Int].run {
    val d: Double = fallible1.extract * 10.0
    val r: Int = fallibleProcess1(d, fallible2.extract).extract
    r + 5
  }
  assert(myEither == Left("Some error"))

def fallible1: Either[String, Double] = Right(1.0)
def fallible2: Either[String, String] = Left("Some error")
def fallibleProcess1(d: Double, s: String): Either[String, Int] = Right(d.toInt)