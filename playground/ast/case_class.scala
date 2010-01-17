class GNode(val name: String, val children: List[GNode])

abstract class Node() {
	def children: List[Node]
}

case class Bar(foo: Foo) extends Node {
	def children = List(foo)
}

case class Foo() extends Node {
	def children = Nil
}

case class WithList(nodes: List[Foo]) extends Node {
	def children = nodes.toList
}

def fromGNode(gnode: GNode): Node = gnode.name match {
	case "Bar" => Bar(fromGNode(gnode.children(0)).asInstanceOf[Foo])
	case "WithList" => WithList(gnode.children
						.map(fromGNode)
						.map(node => node.asInstanceOf[Foo])
					   )
	case "Foo" => Foo()
}

def toGNode(node: Node): GNode = node match {
	case Bar(foo) => new GNode("Bar", List(toGNode(foo)))
	case Foo() => new GNode("Foo", Nil)
	case WithList(nodes) => new GNode("WithList", nodes.map(toGNode))
}

def select(node: Node, predicate: Node=>Boolean): List[Node] =
	 (node.children filter predicate) ::: (node.children flatMap {select(_, predicate)})

val gnode = new GNode("WithList", List(new GNode("Foo", Nil), new GNode("Foo", Nil)))
val node = fromGNode(gnode)

println(node match {
		case WithList(nodes) => "yes"
		case _ => "no"
	}
)

val res = select(node, { case Foo() => true; case _ => false})
println(res)

val gnode2 = toGNode(node)
println(gnode2)
