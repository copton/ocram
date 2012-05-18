package ch.ethz.inf.vs.ruab {

class Ruab (
  val dbginfo: DebugInformation,
  val ecdbg: EcodeDebugger
  )
  extends TcodeDebugger
{
  val debuginfo = dbginfo.load()
  var breakpoints: List[Breakpoint] = Nil

  override def setBreakpoint(line: Int, thread: Thread): Breakpoint = null
  override def deleteBreakpoint(breakpoint: Breakpoint): Unit = ()
  override def listBreakpoints(): List[Breakpoint] = null

  override def query(variable: Variable): String = null
  
  override def currentThread(): Thread = null
  override def listThreads(): List[Thread] = null

  override def backtrace(thread: Thread): Backtrace = null
}

}
