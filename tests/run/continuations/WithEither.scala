import scala.continuations.*

class WithEither[L, R] extends Coroutine[R]:
  type Output = Either[L, R]
  type Extract = Either[L, Any]
  type Suspended[X] = X match
    case Either[l, r] => r

  def process(sm: SM): Either[L, R] =
    def rec(value: Either[L, (Any, sm.State)]): Either[L, Any] = value.flatMap { case (v, s) =>
      if s == sm.finished then Right(v) else
        val (e, ns) = sm.resume(s, v)
        val next = e.get.map(_ -> ns)
        rec(next)
    }

    val (e, ns) = sm.start()
    val start = e.get.map(_ -> ns)
    rec(start).asInstanceOf[Either[L, R]]


extension [L, R](e: Either[L, R]) def extract[F](using c: WithEither[L, F]#C): R /*WithEither[L, F]#Suspended[Either[L, R]]*/ = c.suspend[Either[L, R]](e)