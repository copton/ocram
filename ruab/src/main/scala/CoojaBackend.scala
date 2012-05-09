import ch.ethz.inf.vs.ruab.Types._

package ch.ethz.inf.vs.ruab {

class CoojaBackend() extends EcodeDebugger
{
  override def getSourceLine(): SourceLine = 42
  override def step(): Unit = ()
  override def continue(): Unit = ()
  override def stop(): Unit = ()
  override def setBreakpoint(location: SourceLine): Breakpoint = 23
  override def clearBreakpoint(breakpoint: Breakpoint): Unit = ()
  override def query(variable: String): String = "foo"
    
}
}
