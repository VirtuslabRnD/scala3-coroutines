import scala.continuations.*

class FailingWith[L] extends Executor[Any]:
  type Output[R] = Either[L, R]
  type Extract = Any
  type Suspended[X] = Either[L, X]

  def process[R](sm: Coroutine[R]): Either[L, R] =
    import sm.State.*
    def rec(value: Either[L, (Any, sm.Frame)]): Either[L, Any] = value.flatMap { (v, f) => handle(f.resume(v)) }

    def handle: sm.State => Either[L, Any] =
      case Finished(answer) => Right(answer)
      case Failed(e) => throw e
      case p@ Progressed(v, f) =>
        rec(v.map(_ -> f))

    handle(sm.start()).asInstanceOf[Either[L, R]]


extension [L, R](e: Either[L, R]) inline def extract[F]: R FailWith L = summon[FailingWith[L]#C].suspend[R](e)

inline def fail[L](error: => L): Nothing FailWith L = Left(error).extract

infix type FailWith[+R, L] = FailingWith[L]#C ?=> R
