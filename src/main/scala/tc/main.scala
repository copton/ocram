package tc

object main {
	def main(args: Array[String]) {
		val in = Console.in
		val out = new java.io.OutputStreamWriter(Console.out)

		val gnode = Parser(in)
		println(gnode)

		val node = Transform.encode(gnode)
		println(node)

		val gnode2 = Transform.decode(node)
		println(gnode2)

		Printer(gnode2, out)

		out.flush()
	}	
}
