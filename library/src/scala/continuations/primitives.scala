package scala.continuations

sealed trait Context[Extract, Suspended[_]] {
  def suspend[T <: Extract](suspended: Suspended[T]): T = ??? // marker call
}

trait Executor[A]:
  type Output

  type Extract
  type Suspended[_]

  protected def process(sm: Coroutine): Output

  final type C = scala.continuations.Context[Extract, Suspended]

  final def run(comp: C ?=> A): Output = ??? // marker call

  abstract class Coroutine:
    opaque type StateId = Int
    def start(): State
    def resume(stateId: StateId, t: Extract): State

    enum State:
      case Finished(answer: A)
      case Progressed(value: Suspended[Extract], next: StateId)
      case Failed(exception: Throwable)
