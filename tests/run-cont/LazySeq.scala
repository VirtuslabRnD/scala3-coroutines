import scala.continuations.*

@main def Test =
  val seq = LazyEvaluator[Int].run {
    emit(5)
    emit(8)
    println("done")
  }

  println(seq.iterator.toList)
