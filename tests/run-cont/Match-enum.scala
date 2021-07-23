import scala.collection.*
import scala.continuations.*

enum Fruit:
  case Banana, Cherry, Lime

import Fruit.*

case class FruitSalad(fruits: Fruit*)

extension (s: String)
  // TODO: allow this not to be inline
  inline def asFruit: Fruit FailWith String = s match  
    case "banana" => Banana
    case "carrot" => fail("it's just a carrot")
    case "cherry" => Cherry
    case "lime" => Lime
    case _ =>
      val errorMsg = s"$s is not a fruit"
      fail(errorMsg)

@main def Test =
  println(FailingWith[String].run {
    FruitSalad(
      "banana".asFruit,
      "cherry".asFruit
    )
  })

  println(FailingWith[String].run {
    FruitSalad(
      "lime".asFruit,
      "carrot".asFruit
    )
  })

  println(FailingWith[String].run {
    FruitSalad(
      "beetroot".asFruit,
      "lime".asFruit
    )
  })

  println(FailingWith[String].run {
    FruitSalad(
      "potato".asFruit,
      "leek".asFruit
    )
  })
