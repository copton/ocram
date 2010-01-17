#!/usr/bin/env scala
!#

class GNode(val name: String, val children: List[GNode])

object Foo {
	def unapply(node: GNode): Option[String] = node.name match {
		case "Foo" => Some("hello")
		case _ => None
	}
}

object Bar {
	def unapply(node: GNode): Option[GNode] = node.name match {
		case "Bar" => Some(node.children(0))
		case _ => None
	}
}

val gnode = new GNode("Bar", List(new GNode("Foo", Nil)))

println(gnode match {
		case Bar(Foo(_)) => "yes"
		case _ => "no"
	}
)

