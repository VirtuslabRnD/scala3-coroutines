@main def Test =
  println(FailingWith[String].run{
    val a: 4 | 5 = if true then 4 else 5
    a
  })