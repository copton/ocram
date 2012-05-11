package ch.ethz.inf.vs.ruab {

trait EcodeDebugger {
  type SourceLine = Types.SourceLine
  type Breakpoint = Int
  type Variable = Types.FQI
  type Backtrace = Types.Backtrace

  def getSourceLine(): SourceLine
  def step(): Unit
  def continue(): Unit
  def stop(): Unit
  def setBreakpoint(location: SourceLine): Breakpoint
  def clearBreakpoint(breakpoint: Breakpoint): Unit
  def query(variable: Variable): String
}

}
