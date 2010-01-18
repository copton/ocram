package tc

import xtc.tree.GNode

class RichNode(val name: String, val children: List[Any])
	
abstract case class Node() {
	def children: List[Node]
}

case class TranslationUnit(externalDeclarations: List[ExternalDeclaration], annotations: Node /* Annotations */) extends Node {
	def children = externalDeclarations
}

abstract case class ExternalDeclaration() extends Node
case class Declaration(extension: Boolean, declarationSpecifiers: DeclarationSpecifiers, initializedDeclaratorList: InitializedDeclaratorList) extends ExternalDeclaration {
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
case class SimpleDeclarator(id: String) extends DirectDeclarator {
	def children = Nil
}

// PointerDeclarator extends Declarator

object Transform {
	private def transform[S,T](f: S=>T)(any: Any): Any = any match {
		case null => null
		case s:String => s
		// XXX case s:S  ==> type erasure??? 
		case _ => f(any.asInstanceOf[S])
	}

	object Stage1 {
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

	object Stage2 {
		private val _encode = transform(encode)_
		private val _decode = transform(decode)_

		def encode(node: RichNode): Node = node.name match {
			case "TranslationUnit" => TranslationUnit(
					node.children
					.map(_encode)
					.map(n=>n.asInstanceOf[ExternalDeclaration])
					.init,
					_encode(node.children.last).asInstanceOf[Node] //XXX
					)
				case "Declaration" => Declaration(
						node.children(0) != null, 
						_encode(node.children(1)).asInstanceOf[DeclarationSpecifiers],
						_encode(node.children(2)).asInstanceOf[InitializedDeclaratorList]
						)
				case "DeclarationSpecifiers" => DeclarationSpecifiers(
						node.children
						.map(_encode)
						.map(n=>n.asInstanceOf[DeclarationSpecifier])
						)

				case "VoidTypeSpecifier" => VoidTypeSpecifier()
				case "InitializedDeclaratorList" => InitializedDeclaratorList(
						node.children
						.map(_encode)
						.map(n=>n.asInstanceOf[InitializedDeclarator])
						)
				case "InitializedDeclarator" => {  
					val c = node.children.map(_encode).map(n=>n.asInstanceOf[Node]) // XXX
					InitializedDeclarator(c(0), c(1).asInstanceOf[Declarator], c(2), c(3), c(4))
				}
			case "FunctionDeclarator" => FunctionDeclarator(
					_encode(node.children(0)).asInstanceOf[DirectDeclarator], 
					null // XXX _encode(node.children(1)).asInstanceOf[ParameterTypeList]
					)
				case "SimpleDeclarator" => SimpleDeclarator(node.children(0).asInstanceOf[String])
		}

		def decode(node: Node): RichNode = node match {
			case null => null

				case TranslationUnit(externalDeclarations, annotations) => 
				new RichNode("TranslationUnit", externalDeclarations.map(decode) ::: List(decode(annotations)))

				case Declaration(extension, declarationSpecifiers, initializedDeclaratorList) => 
				new RichNode("Declaration", List(
							if (extension) "__extension__" else null, 
							decode(declarationSpecifiers),
							decode(initializedDeclaratorList)
							))
				case DeclarationSpecifiers(children) => new RichNode("DeclarationSpecifiers", children.map(decode))
				case VoidTypeSpecifier() => new RichNode("VoidTypeSpecifier", Nil)
				case InitializedDeclaratorList(c) => new RichNode("InitializedDeclaratorList", c.map(decode))
				case InitializedDeclarator(c0, c1, c2, c3, c4) => new RichNode("InitializedDeclarator", List(c0, c1, c2, c3, c4).map(decode))
				case FunctionDeclarator(c0, c1) => new RichNode("FunctionDeclarator", List(c0, c1).map(decode))
				case SimpleDeclarator(s) => new RichNode("SimpleDeclarator", List(s))
		}
	}
	def encode(node: GNode): Node = Stage2.encode(Stage1.encode(node))
	def decode(node: Node): GNode = Stage1.decode(Stage2.decode(node))
}
