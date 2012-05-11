package ch.ethz.inf.vs.ruab {

trait DebugSymbols {
  case class Call (
    val caller: String,
    val callee: String
  )

  type CallGraph = List[Call]

  case class Variable(
    val tcodeInstance: Types.FQI,
    val ecodeName: Types.FQI,
    val critical: Boolean
  )

  case class Function (
    val name: String, 
    val tcodeLocation: Types.SourceLine,
    val ecodeLocation: Types.SourceLine,
    val critical: Boolean
  )

  case class Thread (
    val id: Int,
    val threadStartFunction: String,
    val callGraph: CallGraph
  )

  case class Information (
    val functions: List[Function],
    val variables: List[Variable],
    val threads: List[Thread]
  )

  def load(filename: String): Information
}
