@main def Test(): Unit = {
    println(getDocString[Data])
    assert(getDocString[Data].nonEmpty)
}
