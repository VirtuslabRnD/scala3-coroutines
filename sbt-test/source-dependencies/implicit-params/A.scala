class A
{
	implicit def e: E = new E
	def x(i: Int)(implicit y: E): String = ""
}
class E