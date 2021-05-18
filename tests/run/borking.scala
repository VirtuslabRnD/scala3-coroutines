class X:
  abstract class A:
    enum E:
      case Ok(a: Int)
      case NotOk(exception: Throwable)

    opaque type Bork = Int

    def bork(): E

@main def Test =
  val x = new X
  class B extends x.A:
    def bork() =
      val tyryry: Bork = Bork(7)
      E.NotOk(new NullPointerException)

  println