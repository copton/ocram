package tc

object main {
	def main(args: Array[String]) {
		val ast = Parser(Console.in)
		println(ast)
		Printer(ast, Console.out)
	}	
}
