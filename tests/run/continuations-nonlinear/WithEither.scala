import scala.continuations.*

class WithEither[L] extends Executor[Any]:
  type Output[R] = Either[L, R]
  type Extract = Any
  type Suspended[X] = Either[L, X]

  def process[R](sm: Coroutine[R]): Either[L, R] =
    import sm.State.*
    def rec(value: Either[L, (Any, sm.StateId)]): Either[L, Any] = value.flatMap { (v, s) => handle(sm.resume(s, v)) }

    def handle: sm.State => Either[L, Any] =
      case Finished(answer) => Right(answer)
      case Failed(e) => throw e
      case p@ Progressed(v, s) =>
        rec(v.map(_ -> s))

    handle(sm.start()).asInstanceOf[Either[L, R]]


extension [L, R](e: Either[L, R]) inline def extract[F](using c: WithEither[L]#C): R = c.suspend[R](e)

infix type FailWith[R, L] = WithEither[L]#C ?=> R

