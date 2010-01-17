import xtc.tree.GNode

class RichNode(val name: String, val children: List[Any])
	
object RichNode {
	private def convertChildren(gnode: GNode): List[Any] = {
		val iter = gnode.iterator()
		var lst = List[Any]()

		while (iter.hasNext()) {
			lst = iter.next() :: lst
		}
		lst.reverse
	}

	implicit def fromGNode(node: GNode): RichNode = new RichNode(node.getName(), convertChildren(node))

	implicit def toGNode(node: RichNode): GNode = {
		val gnode = GNode.create(node.name, node.children.length)
		for (child <- node.children) gnode.add(child)
		return gnode
	}
}

abstract case class Node() {
	def children: List[Node]
}

case class TranslationUnit(externalDeclarations: List[ExternalDeclaration], annotations: Node /* Annotations */) extends Node {
	def children = externalDeclarations
}

abstract case class ExternalDeclaration() extends Node
case class Declaration(extension: boolean, declarationSpecifiers: DeclarationSpecifiers, initializedDeclaratorList: InitializedDeclaratorList) extends ExternalDeclaration {
	def children = List(declarationSpecifiers, initializedDeclaratorList)
}
// FunctionDefinition extends ExternalDeclaration
// AssemblyDefinition extends ExternalDeclaration
// EmptyDefinition extends ExternalDeclaration

case class DeclarationSpecifiers(children: List[DeclarationSpecifier]) extends Node

abstract case class DeclarationSpecifier() extends Node
abstract case class TypeSpecifier() extends DeclarationSpecifier
case class VoidTypeSpecifier() extends TypeSpecifier { def children = Nil }

//StorageClassSpecifier extends DeclarationSpecifier
//TypeQualifier extends DeclarationSpecifier  
//FunctionSpecifier extends DeclarationSpecifier
//AttributeSpecifier extends DeclarationSpecifier

case class InitializedDeclaratorList(children: List[InitializedDeclarator]) extends Node
case class InitializedDeclarator(attributeSpecifierList_1: Node /*AttributeSpecifierList*/, declarator: Declarator, simpleAssemblyExpression: Node /*SimpleAssemblyExpression*/, attributeSpecifierList_2: Node /*AttributeSpecifierList*/, initializer: Node /*Initializer*/) extends Node {
	def children = List(attributeSpecifierList_1, declarator, simpleAssemblyExpression, attributeSpecifierList_2, initializer)
}

abstract case class Declarator() extends Node
abstract case class DirectDeclarator() extends Declarator 
case class FunctionDeclarator(directDeclarator: DirectDeclarator, parameterTypeList: Node /*ParameterTypeList*/) extends DirectDeclarator {
	def children = List(directDeclarator, parameterTypeList)
}
// @FunctionDeclarator <OldFunction>
// @ArrayDeclarator <FixedArray> extends DirectDeclarator
// @ArrayDeclarator <VariableArray> extends DirectDeclarator
// AttributedDeclarator extends DirectDeclarator 
case class SimpleDeclerator(id: String) extends DirectDeclarator {
	def children = Nil
}

// PointerDeclarator extends Declarator

object Transform {
	def encode(any: Any): Node = any match {
		case null => null
		case node: GNode => encode(node)
	}

	def encode(node: RichNode): Node = node.name match {
		case "TranslationUnit" => TranslationUnit(
									node.children
									.map(encode)
									.map(n=>n.asInstanceOf[ExternalDeclaration])
									.init,
									encode(node.children.last)
								  )
		case "Declaration" => Declaration(
									node.children(0) == null, 
									encode(node.children(1)).asInstanceOf[DeclarationSpecifiers],
									null, //XXX
								)

		case "VoidTypeSpecifier" => VoidTypeSpecifier()
		case "InitializedDeclaratorList" => InitializedDeclaratorList(
												node.children
												.map(encode)
												.map(n=>n.asInstanceOf[InitializedDeclarator])
											)
		case "InitializedDeclarator" => {  
			val c = node.children.map(encode)
			InitializedDeclarator(c(0), c(1).asInstanceOf[Declarator], c(2), c(3), c(4))
		}
		case "FunctionDeclarator" => FunctionDeclarator(
				encode(node.children(0)).asInstanceOf[DirectDeclarator], 
				null, // XXX encode(node.children(1)).asInstanceOf[ParameterTypeList]
			)
		case "SimpleDeclerator" => SimpleDeclerator(node.children(0).asInstanceOf[String])
	}

	def decode(node: Node): GNode = node match {
		case null => null

		case TranslationUnit(externalDeclarations, annotations) => 
			new RichNode("TranslationUnit", List(externalDeclarations.map(decode), decode(annotations)))

		case Declaration(extension, declarationSpecifiers, initializedDeclaratorList) => 
			new RichNode("Declaration", List(
				if (extension) "__extension__" else null, 
				decode(declarationSpecifiers),
				decode(initializedDeclaratorList)
			))
		case VoidTypeSpecifier() => new RichNode("VoidTypeSpecifier", Nil)
		case InitializedDeclaratorList(c) => new RichNode("InitializedDeclaratorList", c.map(decode))
		case InitializedDeclarator(c0, c1, c2, c3, c4) => new RichNode("InitializedDeclarator", List(c0, c1, c2, c3, c4).map(decode))
		case FunctionDeclarator(c0, c1) => new RichNode("FunctionDeclarator", List(c0, c1).map(decode))
		case SimpleDeclerator(s) => new RichNode("SimpleDeclerator", List(s))
	}
}
