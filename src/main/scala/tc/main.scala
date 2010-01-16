package tc

object main {
	def main(args: Array[String]) {
		val in = Console.in
		val out = new java.io.OutputStreamWriter(Console.out)

		val ast = Parser(in)
		println(ast)
		Printer(ast, out)

		out.flush()
	}	
}
