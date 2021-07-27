package kind_projector

trait Foo[F[_]]
trait Qux[F[_, _]]
trait Baz[F[_], A, B]

trait FooPlus[+F[+_]]
trait QuxPlus[+F[+_, +_]]
trait BazPlus[+F[+_], +A, +B]

trait FooMinus[-F[-_]]
trait QuxMinus[-F[-_, -_]]
trait BazMinus[-F[-_], -A, -B]

class Bar1 extends Foo[Either[Int, *]]
class Bar2 extends Foo[Either[*, Int]]
class Bar3 extends Foo[* => Int]
class Bar4 extends Foo[Int => *]
class Bar5 extends Foo[(Int, *, Int)]
class Bar6 extends Foo[λ[x => Either[Int, x]]]
class Bar7 extends Qux[λ[(x, y) => Either[y, x]]]
class Bar8 extends Foo[Baz[Int => *, *, Int]]
class Bar9 extends Foo[λ[x => Baz[x => *, Int, x]]]

class BarPlus1 extends FooPlus[Either[Int, +*]]
class BarPlus2 extends FooPlus[Either[+*, Int]]
class BarPlus3 extends FooPlus[Int => +*]
class BarPlus4 extends FooPlus[(Int, +*, Int)]
class BarPlus5 extends FooPlus[λ[`+x` => Either[Int, x]]]
class BarPlus6 extends QuxPlus[λ[(`+x`, `+y`) => Either[y, x]]]
class BarPlus7 extends FooPlus[BazPlus[Int => +*, +*, Int]]

class BarMinus1 extends FooMinus[-* => Int]

class VarianceAnnotationIsActuallyIgnored1 extends FooPlus[Either[Int, -*]]
class VarianceAnnotationIsActuallyIgnored2 extends FooPlus[Either[-*, Int]]
class VarianceAnnotationIsActuallyIgnored3 extends FooMinus[+* => Int]
class VarianceAnnotationIsActuallyIgnored4 extends FooPlus[Int => -*]
class VarianceAnnotationIsActuallyIgnored5 extends FooPlus[(Int, -*, Int)]
class VarianceAnnotationIsActuallyIgnored6 extends FooPlus[λ[`-x` => Either[Int, x]]]
class VarianceAnnotationIsActuallyIgnored7 extends QuxPlus[λ[(`-x`, `-y`) => Either[y, x]]]
class VarianceAnnotationIsActuallyIgnored8 extends FooPlus[BazPlus[Int => -*, -*, Int]]
class VarianceAnnotationIsActuallyIgnored9 extends Foo[λ[`-x` => BazPlus[x => -*, Int, x]]]
