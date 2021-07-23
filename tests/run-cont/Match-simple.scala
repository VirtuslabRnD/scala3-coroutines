import scala.collection.*
import scala.continuations.*

inline def success[R](right: => R): R FailWith String = Right(right).extract

extension (i: Int)
  def assertPositive: Int FailWith String = if i > 0 then i else fail("Not positive")

@main def Test =
  // no suspension 
  println(FailingWith[String].run {
    1 + 2 match
      case 0 => 0
      case n if n >= 0 => n
  })

  // succeeding suspension in scrutinee
  println(FailingWith[String].run {
    {
      val x = success(4)
      x + 1
    } match
      case 0 => 0
      case n if n >= 0 => n
  })

  // failing suspension in scrutinee
  println(FailingWith[String].run {
    {
      val x: Int = -1
      x.assertPositive
    } match
      case 0 => 0
      case n if n >= 0 => n
  })

  // succeeding suspension in guard
  println(FailingWith[String].run {
    val x = 2
    x + 1 match
      case 0 => 0
      case n if { val m = n.assertPositive; m % 2 == 1 } => n // TODO: Use simply: n.assertPositive % 2 == 1
  })

  // failing suspension in guard
  println(FailingWith[String].run {
    val x = -10
    x + 1 match
      case 0 => 0
      case n if { val m = n.assertPositive; m % 2 == 1 } => n // TODO: Use simply: n.assertPositive % 2 == 1
  })

  // succeeding suspension in case body
  println(FailingWith[String].run {
    val x = 0
    x + 1 match
      case 0 => 0
      case n => n.assertPositive
  })

  // failing suspension in case body
  println(FailingWith[String].run {
    val x = 0
    x - 1 match
      case 0 => 0
      case n => n.assertPositive
  })

  // match on match result: both suspensions succeeding
  println(FailingWith[String].run {
    success(1) match
      case 0 =>
      case n =>
        success(n) match
          case 1 => 1
  })

  // match on match result: first suspension failing
  println(FailingWith[String].run {
    fail("Rollback") match
      case 0 =>
      case n =>
        success(n) match
          case 1 => 1
  })

  // match on match result: second suspension failing
  println(FailingWith[String].run {
    success(1) match
      case 0 =>
      case n =>
        fail("Rollback") match
          case 1 => 1
  })

  // match on match result: both suspensions failing
  println(FailingWith[String].run {
    fail("Rollback 1") match
      case 0 =>
      case n =>
        fail("Rollback 2") match
          case 1 => 1
  })
