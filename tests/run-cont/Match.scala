import scala.collection.*
import scala.continuations.*

inline def success[R](right: => R): R FailWith String = Right(right).extract

enum Fruit:
  case Banana, Cherry, Lime
import Fruit.*
extension (s: String)
  // inline def asFruit: Fruit FailWith String =    
    // if s == "banana" then Banana
    // else if s == "cherry" then Cherry
    // else if s == "lime" then Lime
    // else
    //   val errorMsg = s"$s is not a fruit"
    //   fail(errorMsg)
  inline def asFruit: Fruit FailWith String = s match  
    case "banana" => Banana
    case "carrot" => fail("it's just a carrot")
    case "cherry" => Cherry
    case "lime" => Lime
    case _ =>
      val errorMsg = s"$s is not a fruit"
      fail(errorMsg)

// def maybeFruit(fruitName: String): Either[String, Fruit] = FailingWith[String].run {
//   fruitName.asFruit
// }

case class FruitSalad(fruits: Fruit*)

// def makeSalad(fruitNames: String*): Either[String, FruitSalad] = FailingWith[String].run {
//   FruitSalad(fruitNames.map(_.asFruit): _*)
// }



// extension (s: String)
//   inline def asBoolean: Boolean FailWith String =
//     if s == "true" then true
//     else if s == "false" then false
//     else fail("not a boolean")

// def maybeBoolean(s: String): Either[String, Boolean] = 
//   FailingWith[String].run(s.asBoolean)



object SucceedingExtractor1 {
  def unapply(n: Int)(using FailingWith[String]#C): Option[Int] = success(Some(n))
}

object SucceedingExtractor2 {
  def unapply(n: Int)(using FailingWith[String]#C): Option[Int] = Some(success(n))
}

object FailingExtractor1 {
  def unapply(n: Int)(using FailingWith[String]#C): Option[Int] = Some(fail("Failing extractor"))
}

@main def Test =
  // println(FailingWith[String].run {
  //   1 + 2 match
  //     case n: Int if n >= 0 => n
  //     case n => -n
  // })

  println(FailingWith[String].run {
    1 + 2 match
      case n if n >= 0 => n
      case n =>
        val errorMsg = s"$n is negative"
        fail(errorMsg)
  })

  // println(FailingWith[String].run {
  //   1 - 2 match
  //     case n if n >= 0 => n
  //     case n =>
  //       val errorMsg = s"$n is negative"
  //       fail(errorMsg)
  // })

  // println(FailingWith[String].run {
  //   1 + 2 match
  //     case n if n >= 0 =>
  //       val x = n + 1
  //       success(x) match
  //         case n if n % 2 == 0 => 1
  //         case n => -1 
  // })

  // println(FailingWith[String].run {
  //   success(1 + 1) match
  //     case 2 => 2
  // })

  // println(FailingWith[String].run {
  //   fail("Don't match on me!") match
  //     case 2 => 2
  // })

  // println(FailingWith[String].run {
  //   Some(1) match
  //     case Some(n) => n
  // })

  // println(FailingWith[String].run {
  //   Some(success(1)) match
  //     case Some(n) => n
  // })

  // println(FailingWith[String].run {
  //   success(Some(1)) match
  //     case Some(n) => n
  // })

  // println(FailingWith[String].run {
  //   1 match
  //     case SucceedingExtractor1(n) => n
  // })

  // println(FailingWith[String].run {
  //   1 match
  //     case SucceedingExtractor2(n) => n
  // })

  // println(FailingWith[String].run {
  //   1 match
  //     case FailingExtractor1(n) => n
  // })



  // println(FailingWith[String].run {
  //   fruitName.asFruit
  //   // "banana" match
  //   //   case "banana" => Banana
  //   //   case "cherry" => Cherry
  //   //   case "lime" => Lime
  //   //   case _ =>
  //   //     // val errorMsg = s"$s is not a fruit"
  //   //     val errorMsg = s"not a fruit"
  //   //     fail(errorMsg)
  //   //     // Lime
    
  // })

  // println((new FailingWith[String]).run {
  //   success(1) match
  //     // case n => (fail("booo"): Int) + 1
  //     case n =>
  //       val s = success(1)
  //       s + 1
  // })

  // println(FailingWith[String].run {
  //   FruitSalad(
  //     "banana".asFruit,
  //     "cherry".asFruit
  //   )
  // })

  // println(FailingWith[String].run {
  //   FruitSalad(
  //     "lime".asFruit,
  //     "carrot".asFruit
  //   )
  // })

  // println(FailingWith[String].run {
  //   FruitSalad(
  //     "potato".asFruit,
  //     "carrot".asFruit
  //   )
  // })

  // println(salad1)
  // println(salad2)
  // println(salad3)