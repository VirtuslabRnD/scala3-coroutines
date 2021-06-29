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

  abstract class Coroutine[TA]:
    protected[Executor] def startInternal(): StackChange
    protected[Executor] def resumeInternal(stateId: Int, extract: Any): StackChange

    enum State:
      case Finished(answer: TA)
      case Progressed(value: Suspended[Extract], stack: Frame)
      case Failed(exception: Throwable)

    import StackChange.*
    import State.*

    case class Frame(sm: Coroutine[?], stateId: Int, parent: Frame):
      def start(next: Coroutine[?]): State = next.startInternal() match
        case Pop(value) => resumeInternal(value)
        case Push(next2, stateId) => Frame(next, stateId, this).start(next2)
        case Progress(value, stateId)  => Progressed(value, Frame(next, stateId, this))
        case Fail(e) => Failed(e)

      inline def resume(extract: Extract): State = resumeInternal(extract)

      private def resumeInternal(extract: Any): State = sm.resumeInternal(stateId, extract) match
        case Pop(value) if parent eq null => Finished(value.asInstanceOf[TA])
        case Pop(value) => parent.resumeInternal(value)
        case Push(next, stateId) => goTo(stateId).start(next)
        case Progress(value, stateId) => Progressed(value, goTo(stateId))
        case Fail(e) => Failed(e)

      private def goTo(stateId: Int) = copy(stateId = stateId)

    def start(): State = startInternal() match
      case Pop(value) => Finished(value.asInstanceOf[TA])
      case Push(next, stateId) => Frame(this, stateId, null).start(next)
      case Progress(value, stateId) => Progressed(value, Frame(this, stateId, null))
      case Fail(e) => Failed(e)


  enum StackChange:
    case Pop(value: Any)
    case Push(sm: Coroutine[?], stateId: Int)
    case Progress(value: Suspended[Extract], stateId: Int)
    case Fail(exception: Throwable)
