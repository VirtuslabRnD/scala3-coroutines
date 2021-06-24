import scala.collection.*
import scala.continuations.*

// extension (s: String)
//   inline def asBoolean: Boolean FailWith String =
//     val result: Boolean = 
//       if s == "true" then true
//       else if s == "false" then false
//       else fail("not a boolean")
//     result

// def maybeBoolean(s: String): Either[String, Boolean] = 
//   FailingWith[String].run(s.asBoolean)

// def show(f: /* => */ Int FailWith String) =
//   FailingWith[String].run(f)

// def maybeInt(i: Int): Either[String, Int] = FailingWith[String].run(i.asOne)

extension (i: Int)
  inline def same: Int FailWith String = 
    i match {
      case n => i
      case _ => i
    }

  // inline def assertEqOne: Int FailWith String =
  //   i match {
  //     case 1 => 1
  //     case n =>
  //       val errorMsg = s"$i is not one"
  //       fail(errorMsg)
  //       //fail("It's not one")
  //   }

@main def Test =
  println(FailingWith[String].run {
    0.same
  })
  // println(FailingWith[String].run {
  //   1.assertEqOne
  // })
  // println(FailingWith[String].run {
  //   2.assertEqOne
  // })
  // println(FailingWith[String].run {
  //   fail("some error").assertEqOne
  // })
  // println(FailingWith[String].run {
  //   Right(1).extract.assertEqOne
  // })