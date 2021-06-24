import scala.continuations.*

def fallible1: Either[String, Double] = Right(1)
inline def asInt(a: Double): Int FailWith String = if a.isWhole then a.toInt else fail("Not whole")
//def 
inline def success[R](right: => R): R FailWith String = Right(right).extract
//inline def maybeFail[L](error: => L): Nothing FailWith L = Left(error).extract

//inline def crash: Nothin

@main def Test =
  // println((new Optionally).run {
  //   val x = 1
  //   x + 1
  // })

  // println((new FailingWith[String]).run {
  //   val x = fail("ZZZZZ"): Int
  //   x + 1
  // })

  // println((new FailingWith[String]).run {
  //   val x = fail("ZZZZZ"): Int

  //   x match
  //     case _ => x + 1
  // })

  // println((new FailingWith[String]).run {
  //   val x = fail("ZZZZZ"): Int

  //   x match
  //     case n => x + 1
  // })

  // println((new FailingWith[String]).run {
  //   val x = fail("ZZZZZ"): Int

  //   x match
  //     case n => n + 1
  // })

  // println((new FailingWith[String]).run {
  //   (fail("ZZZZZ"): Int) match
  //     case n => n + 1
  // })

  // println((new FailingWith[String]).run {
  //   success(1) match
  //     case n => n + 1
  // })

  println((new FailingWith[String]).run {
    success(1) match
      // case n => (fail("booo"): Int) + 1
      case n =>
        val s = success(1)
        s + 1
  })

  // println((new FailingWith[String]).run {
  //   success(1) match
  //     // case n => (fail("booo"): Int) + 1
  //     case 1 => 0
  //     case 2 => -2
  //     case n => 1 + n
  // })

  // println((new FailingWith[String]).run {
  //   val x: Int = 
  //     if success(1) == 1 then
  //       success(1) + 1
  //     else
  //       -1//throw new Exception("zzz")
  //   x
  // })

  // println(FailingWith[String].run {
  //   if Right(1.0).extract.isWhole then "foo" else "bar"
  // })

  // println(FailingWith[String].run {
  //   if Right(1.0).extract.isWhole == true then "foo" else "bar"
  // })




  // println((new Optionally).run {
  //   val x = Some(1).extractOpt
  //   x + 1
  // })

  // println((new Optionally).run {
  //   val x: Int = none.extractOpt: Int
  //   x + 1
  // })

  // println((new Optionally).run {
  //   val (Some(x), y) = Some((Some(1), 2)).extractOpt
  //   x + 1
  // })



  // println((new FailingWith[String]).run {
  //   val x: Int = fail("ZZZZZ"): Int
  //   x + 1
  // })