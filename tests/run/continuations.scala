import scala.collection.*
import scala.continuations.*

class LazySeq[A](sm: Lazily[A]#SM) extends IterableOnce[A]:
  private var state: Option[sm.State] = None
  private var _next: Option[A] = None

  def iterator = new Iterator[A]:
    def hasNext =
      if _next.nonEmpty then true else
        val (value, nstate) = state.map(s => sm.resume(s, ())).getOrElse(sm.start())
        state = Some(nstate)
        value match
          case None => false
          case _ =>
            _next = value
            true

    def next() =
      if _next.isEmpty then hasNext
      val res = _next.get
      _next = None
      res

class Lazily[T] extends Coroutine[Unit]:
  type Output = [_] =>> LazySeq[T]
  type Extract = T
  type Suspended = [_] =>> Unit

  def process(sm: SM): LazySeq[T] = LazySeq(sm)


@main def Test =
  val mySeq = Lazily[Int].run {
    summon[Lazily[Int]#C].suspend(6)
    summon[Lazily[Int]#C].suspend(7)
    summon[Lazily[Int]#C].suspend(8)
    summon[Lazily[Int]#C].suspend(9)
    summon[Lazily[Int]#C].suspend(10/0)
  }
  println(mySeq.iterator.take(3).toList)
