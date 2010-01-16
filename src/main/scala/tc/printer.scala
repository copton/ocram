package tc

object Printer {
	def apply(ast: xtc.tree.GNode, out: java.io.PrintStream) {
		val printer = new xtc.lang.CPrinter(new xtc.tree.Printer(out))
		printer.dispatch(ast)
		out.flush()
	}
}
