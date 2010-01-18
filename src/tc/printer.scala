package tc

object Printer {
	def apply(ast: xtc.tree.GNode, out: java.io.Writer) {
		val printer = new xtc.lang.CPrinter(new xtc.tree.Printer(out))
		printer.dispatch(ast)
	}
}
