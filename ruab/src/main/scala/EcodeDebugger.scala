import ch.ethz.inf.vs.ruab.Types._

package ch.ethz.inf.vs.ruab {

  trait EcodeDebugger {
    def getSourceLine(): SourceLine
    def step(): Unit
    def continue(): Unit
    def stop(): Unit
    def setBreakpoint(location: SourceLine): Breakpoint
    def clearBreakpoint(breakpoint: Breakpoint): Unit
    def query(variable: String): String
  }

  object EcodeDebugger {
    def create(choice: String) : EcodeDebugger = choice match {
      case "cooja" => new CoojaBackend
    }
  }
}
