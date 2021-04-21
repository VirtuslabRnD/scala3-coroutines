package scala.continuations

sealed trait Context[Extract, Suspended[_]] {
  def suspend[T <: Extract](extract: T): Suspended[T] = ??? // marker call
}

trait Coroutine[A]:
  type Output[_]

  type Extract
  type Suspended[_]

  protected def process(sm: SM): Output[A]

  final type C = scala.continuations.Context[Extract, Suspended]

  final def run(comp: C ?=> A): Output[A] = ??? // marker call

  abstract class SM:
    opaque type State = Int
    final val initialState: State = 0
    final val finished: State = -1

    def start(): (Option[Extract], State)
    def resume(state: State, t: Suspended[Extract]): (Option[Extract], State)
