package test.ast

import tc.ast.Ast

object main {
	def main(args: Array[String]) {
		val in = Console.in
		val out = new java.io.OutputStreamWriter(Console.out)

		val gnode = tc.Parser(in)
		println("in:")
		println(format(gnode.toString))

		val node = Ast.encode(gnode)
		println("intermediate:")
		println(format(node.toString))

		val gnode2 = Ast.decode(node)
		println("out:")
		println(format(gnode2.toString))

		tc.Printer(gnode2, out)

		out.flush()
	}	
}
