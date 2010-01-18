package test.ast

object main {
	def main(args: Array[String]) {
		val in = Console.in
		val out = new java.io.OutputStreamWriter(Console.out)

		val gnode = tc.Parser(in)
		println(gnode)

		val node = tc.Transform.encode(gnode)
		println(node)

		val gnode2 = tc.Transform.decode(node)
		println(gnode2)

		tc.Printer(gnode2, out)

		out.flush()
	}	
}
