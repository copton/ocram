import java.lang.Integer

package ch.ethz.inf.vs.ruab {
  object Types {
    type Breakpoint = Integer
    type SourceLine = Integer

    abstract class FQI( // fully-qualified identifier
      val name: String
    )
    case class GlobalIdentifier(override val name: String) extends FQI(name)
    case class ScopedIdentifier(
      override val name: String,
      val function: String,
      val scope: List[java.lang.Integer]
    ) extends FQI(name)

    class Variable(
      val tcodeInstance: FQI,
      val ecodeName: FQI,
      val critical: Boolean
    )

    class Function (
      val name: String, 
      val tcodeLocation: SourceLine,
      val ecodeLocation: SourceLine,
      val critical: Boolean
    ) { }  

    class Call (
      val caller: String,
      val callee: String
    ) { }

    type CallGraph = List[Call]

    class Thread (
      val id: Integer,
      val threadStartFunction: String,
      val callGraph: CallGraph
    ) { }
  }
}
