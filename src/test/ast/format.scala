package test.ast

object format {
	def apply(in: String): String = {
		val out = new StringBuilder(in.length)
		var indent=0
		def newline() { out.append('\n'); out.append(" " * (4 * indent)) }

		for (i <- 0 until in.length) {
			val c = in(i)
			c match {
				case '(' => in(i+1) match {
					case ')' => out.append(c)
					case  _  => {indent += 1; out.append(c); newline()}
				}
				case ')' => in(i-1) match {
					case '(' => out.append(c)
					case _ => {indent -= 1; newline(); out.append(c)}
				}
				case ',' => {out.append(c); newline()}
				case ' ' => ()
				case  _  => out.append(c)
			}		
		}

		out.toString
	}
}

