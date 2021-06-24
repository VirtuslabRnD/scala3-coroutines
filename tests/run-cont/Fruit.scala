import scala.collection.*
import scala.continuations.*

enum Fruit:
  case Banana, Cherry, Lime

import Fruit.*

case class FruitSalad(fruits: Fruit*)

case class FruitPair(fruit1: Fruit, fruit2: Fruit)

extension (s: String)
  inline def asFruit: Fruit FailWith String =    
    if s == "banana" then Banana
    else if s == "cherry" then Cherry
    else if s == "lime" then Lime
    else fail("not a fruit")

def fruitPair(fruitName1: String, fruitName2: String): Either[String, FruitPair] =
  FailingWith[String].run {
    val fruit1: Fruit = fruitName1.asFruit
    val fruit2: Fruit = fruitName2.asFruit
    FruitPair(
      fruit1,
      fruit2
    )
  }

@main def Test =
  println(fruitPair("banana", "cherry"))

  // val salad1 = FailingWith[String].run {
  //   FruitSalad(
  //     "banana".asFruit,
  //     "cherry".asFruit
  //   )
  // }

  // val salad2 = FailingWith[String].run {
  //   FruitSalad(
  //     "lime".asFruit,
  //     "carrot".asFruit
  //   )
  // }

  // val salad2 = FailingWith[String].run {
  //   val banana: Fruit = "banana".asFruit
  //   val cherry: Fruit = "cherry".asFruit
  //   val fruits: Seq[Fruit] = Seq(banana, cherry)
  //   //Seq(banana, cherry)
  //   // FruitSalad(
  //   //   banana,
  //   //   cherry
  //   // )
  // }

  // val salad3 = FailingWith[String].run {
  //   val carrot: Fruit = "carrot".asFruit
  //   val lime: Fruit = "lime".asFruit
  //   FruitPair(carrot, lime)
  // }

  // val salad4 = FailingWith[String].run {
  //   //val carrot: Fruit = "carrot".asFruit
  //   //val lime: Fruit = "lime".asFruit
  //   FruitPair(
  //     "carrot".asFruit,
  //     "lime".asFruit
  //   )
  // }
