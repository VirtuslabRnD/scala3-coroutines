import scala.continuations.*

inline def fill[A](n: Int)(a: A): Seq[A] FailWith String = 
  if n >= 0 then
    Seq.fill(n)(a) 
  else
    fail("Cannot repeat an element a negative number of times")

@main def Test =
  println(FailingWith[String].run{
    fill(3)("abc")
  })

  println(FailingWith[String].run{
    fill(-1)("abc")
  })
