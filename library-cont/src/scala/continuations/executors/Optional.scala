import scala.continuations.*
import scala.annotation.targetName

class Optionally extends Executor[Any]:
  type Output[R] = Option[R]
  type Extract = Any
  type Suspended[X] = Option[X]

  def process[R](sm: Coroutine[R]): Option[R] =
    import sm.State.*
    def rec(value: Option[(Any, sm.StateId)]): Option[Any] = value.flatMap { (v, s) => handle(sm.resume(s, v)) }

    def handle: sm.State => Option[Any] =
      case Finished(answer) => Some(answer)
      case Failed(e) => throw e
      case p@ Progressed(v, s) =>
        rec(v.map(_ -> s))

    handle(sm.start()).asInstanceOf[Option[R]]


extension [R](o: Option[R])
  //@targetName("extractOption")
  inline def extractOpt[F]: Optional[R] = summon[Optionally#C].suspend[R](o)

inline def none[L]: Optional[Nothing] = None.extractOpt

infix type Optional[+R] = Optionally#C ?=> R
