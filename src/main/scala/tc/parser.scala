package tc

import xtc.lang.CParser
import xtc.tree.GNode

import xtc.parser.{Result,SemanticValue,ParseError}

class ParseException(what: String) extends Exception(what)

object Parser {
	def apply(in: java.io.BufferedReader): GNode = {
		val cparser = new CParser(in, "unknown")
		val result = cparser.pTranslationUnit(0)
		if (result.hasValue()) {
			GNode.cast(result.asInstanceOf[SemanticValue].value)
		} else {
			val err = result.asInstanceOf[ParseError]
			if (err.index == -1) {
				throw new ParseException("parsing failed")
			} else {
				throw new ParseException("parsing failed: " + cparser.location(err.index) + ": " + err.msg)
			}	
		}
	}
}
