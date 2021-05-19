@main def Test =
  val generator = Lazily[Int].run {
    give(2+4)
    for x <- 7 to 9 do give(x)
    give(10 / 0)
  }

  assert(generator.iterator.take(3).toList == List(6, 7, 8))