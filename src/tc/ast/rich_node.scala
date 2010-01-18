package tc.ast

import xtc.tree.GNode

class RichNode(val name: String, val children: List[Any])

object RichNode {
	private def transform[S,T](f: S=>T)(any: Any): Any = any match {
		case null => null
		case s:String => s
		// XXX case s:S  ==> type erasure??? 
		case _ => f(any.asInstanceOf[S])
	}

	private val _encode = transform(encode)_
	private val _decode = transform(decode)_

	def encode(node: GNode): RichNode = {
		val iter = node.iterator()
			var children = List[Any]()

			while (iter.hasNext()) {
				val next = _encode(iter.next())
					children = children ::: List(next)
			}

		new RichNode(node.getName(), children)
	}

	def decode(node:RichNode): GNode = {
		val gnode = GNode.create(node.name, node.children.length)
		for (child <- node.children) {
			gnode.add(_decode(child))
		}
		return gnode
	}
}
