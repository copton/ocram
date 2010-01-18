package tc.ast

import xtc.tree.GNode

object Ast {
	def encode(node: GNode): Node = Node.encode(RichNode.encode(node))
	def decode(node: Node): GNode = RichNode.decode(Node.decode(node))
}
