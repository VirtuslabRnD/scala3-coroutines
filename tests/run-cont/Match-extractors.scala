import scala.collection.*
import scala.continuations.*

inline def success[R](right: => R): R FailWith String = Right(right).extract

object SucceedingExtractor1:
  def unapply(n: Int)(using FailingWith[String]#C): Option[Int] =
    val s = Some(n)
    success(s)

object SucceedingExtractor2:
  def unapply(n: Int)(using FailingWith[String]#C): Option[Int] = Some(success(n))

object FailingExtractor1:
  def unapply(n: Int)(using FailingWith[String]#C): Option[Int] = None

object FailingExtractor2:
  def unapply(n: Int)(using FailingWith[String]#C): Option[Int] = Some(fail("Failing extractor"))


@main def Test =
  println(FailingWith[String].run {
    1 match
      case SucceedingExtractor1(n) => n
  })

  println(FailingWith[String].run {
    2 match
      case SucceedingExtractor2(n) => n
  })

  println(FailingWith[String].run {
    3 match
      case SucceedingExtractor2(n) if { fail("Rollback"); n < 0 } => n
  })

  println(FailingWith[String].run {
    4 match
      case FailingExtractor1(n) => n
      case _ => 0
  })

  println(FailingWith[String].run {
    5 match
      case FailingExtractor2(n) => n
  })
