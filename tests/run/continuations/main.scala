import scala.collection.*
import scala.continuations.*

class A:
  class B

@main def Test =
  // val mySeq = Lazily[Int].run {
  //   give(2+4)
  //   give(7)
  //   give(8)
  //   // for x <- 7 to 9 do give(x)
  //   give(10 / 0)
  // }
  // assert(mySeq.iterator.take(3).toList == List(6, 7, 8))

  val myEither = WithEither[String, Int].run {
    val d: Double = 10 * fallible1.extract
    val r: Int = fallibleProcess1(d, fallible2.extract).extract
    println("I'm here?")
    r + 5
  }

  val myEither2 = WithEither[String, Int].run {
    val d: Double = 10 * fallible1.extract
    val r: Int = fallibleProcess1(d, fallible3.extract).extract
    println("I'm here!")
    println("And r = " + r)
    r + 5
  }

  println(myEither)
  println(myEither2)

def fallible1: Either[String, Double] = Right(1.0)
def fallible2: Either[String, String] = Left("Some error")
def fallible3: Either[String, String] = Right("Not error")
def fallibleProcess1(d: Double, s: String): Either[String, Int] = Right(d.toInt)