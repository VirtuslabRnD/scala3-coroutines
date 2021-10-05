import scala.continuations.*
import scala.util.chaining.*

class LazySeq[T](coroutine: LazyEvaluator[T]#Coroutine[? <: Unit]) extends IterableOnce[T]:
  def iterator = new Iterator[T]:
    private var step: Option[T] = None
    private var frame: Option[coroutine.Frame] = None

    private def tryAdvance() = step match
      case Some(_) => ()
      case None    => handle(frame match
        case Some(f) => f.resume(())
        case None    => coroutine.start())

    private def handle(state: coroutine.State) =
      import coroutine.State.*
      state match
        case Finished(_) =>
          frame = None
          step = None
        case Progressed(value, continuation) =>
          step = Some(value)
          frame = Some(continuation)
        case Failed(e) => throw e

    def hasNext =
      tryAdvance()
      step.isDefined

    def next() =
      tryAdvance()
      step.get.tap {_ => step = None }
end LazySeq

class LazyEvaluator[T] extends Executor[Unit]:
  type Output[_] = LazySeq[T]
  type Extract = Unit
  type Suspended[_] = T
  def process[TA <: Unit](coroutine: Coroutine[TA]) = LazySeq[T](coroutine)

inline def emit[T](t: T)(using evaluator: LazyEvaluator[T]#C): Unit = evaluator.suspend(t)

type Lazily[T] = LazyEvaluator[T]#C ?=> Unit

// transparent inline def lazily[T](inline coroutine: Lazily[T]) = LazyEvaluator[T].run(coroutine)
