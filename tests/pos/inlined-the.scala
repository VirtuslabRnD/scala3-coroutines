object Instances {

  class D[T]

  class C {
    def f() = {
      locally {
        given d[T]: D[T]()
        summon[D[Int]]
        implicit val s: 3 = ???
        val a: 3 = summon[3]
        val b: s.type = summon[s.type]
        ()
      }

      locally {
        given d[T]: D[T]()
        the2[D[Int]]
        implicit val s: 3 = ???
        val a: 3 = the2[3]
        val b: s.type = the2[s.type]
        ()
      }

      locally {
        implicit val s: List[3] = ???
        val a: List[3] = the2[List[3]]

        implicit val sl: List[s.type] = ???
        val b: List[s.type] = the2[List[s.type]]
        ()
      }
    }
  }

  inline def the2[T](implicit x: T): x.type = x

  inline def theList[T](implicit x: T): List[x.type] = List[x.type](x)

}
