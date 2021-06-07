package scala.continuations

sealed trait Context[Extract, Suspended[_]] {
  def suspend[T <: Extract](suspended: Suspended[T]): T = ??? // marker call
}

trait Executor[A]:
  type Output[_]

  type Extract
  type Suspended[_]

  protected def process[TA <: A](sm: Coroutine[TA]): Output[TA]

  final type C = scala.continuations.Context[Extract, Suspended]

  final def run[TA <: A](comp: C ?=> TA): Output[TA] = ??? // marker call

  abstract class Coroutine[TA <: A]:
    type StateId = Int
    def start(): State
    def resume(stateId: StateId, t: Extract): State

    enum State:
      case Finished(answer: TA)
      case Progressed(value: Suspended[Extract], next: StateId)
      case Failed(exception: Throwable)
