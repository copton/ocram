import ch.ethz.inf.vs.ruab.Types._

package ch.ethz.inf.vs.ruab {

  class DebugSymbols (
    val functions: List[Function],
    val variables: List[Variable],
    val threads: List[Thread]
  ) { }
  
  object DebugSymbols {
    def load(): DebugSymbols = new DebugSymbols(Nil, Nil, Nil)
  }
}
