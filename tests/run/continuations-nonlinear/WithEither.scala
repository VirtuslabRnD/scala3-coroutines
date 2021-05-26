import scala.continuations.*

class WithEither[L, R] extends Executor[R]:
  type Output = Either[L, R]
  type Extract = Any
  type Suspended[X] = Either[L, X]

  def process(sm: Coroutine): Either[L, R] =
    import sm.State.*
    def rec(value: Either[L, (Any, sm.StateId)]): Either[L, Any] = value.flatMap { (v, s) => handle(sm.resume(s, v)) }

    def handle: sm.State => Either[L, Any] =
      case Finished(answer) => Right(answer)
      case Failed(e) => throw e
      case p@ Progressed(v, s) =>
        rec(v.map(_ -> s))

    handle(sm.start()).asInstanceOf[Either[L, R]]


extension [L, R](e: Either[L, R]) inline def extract[F](using c: WithEither[L, F]#C): R = c.suspend[R](e)