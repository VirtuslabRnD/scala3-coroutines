import scala.collection.*
import scala.continuations.*

inline def show(inline f: Int FailWith String): Either[String, Int] =
  FailingWith[String].run(f)

@main def Test =
  ()