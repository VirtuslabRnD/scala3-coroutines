import scala.continuations.*

extension (s: String)
  inline def asBoolean: Boolean FailWith String =
    if s == "true" then true
    else if s == "false" then false
    else fail("not a boolean")

def maybeBoolean(s: String): Either[String, Boolean] = 
  FailingWith[String].run(s.asBoolean)

@main def Test =
  println(maybeBoolean("true"))