import scala.collection.*
import scala.continuations.*

class LazySeq[A](sm: Lazily[A]#Coroutine) extends IterableOnce[A]:
  private var state: Option[sm.StateId] = None
  private var _next: Option[A] = None

  def iterator = new Iterator[A]:
    def hasNext =
      import sm.State.*
      if _next.nonEmpty then true else
        state.map(s => sm.resume(s, ())).getOrElse(sm.start()) match
          case Failed(e) => throw e
          case Finished(_) => false
          case Progressed(v, s) =>
            state = Some(s)
            _next = Some(v)
            true

    def next() =
      if _next.isEmpty then hasNext
      val res = _next.get
      _next = None
      res

class Lazily[T] extends Executor[Unit]:
  type Output = LazySeq[T]
  type Extract = Unit
  type Suspended = [_] =>> T

  def process(sm: Coroutine): LazySeq[T] = LazySeq(sm)

inline def give[T](value: T)(using c: Lazily[T]#C): Unit = c.suspend(value)