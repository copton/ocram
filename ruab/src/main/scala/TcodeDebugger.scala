package ch.ethz.inf.vs.ruab {

trait TcodeDebugger {
  case class Thread(id: Int, threadStartFunction: String)
  case class Breakpoint(id: Int, thread: Thread)
  type Variable = Types.FQI 
  type Backtrace = Types.Backtrace
  type SourceLine = Types.SourceLine

  def setBreakpoint(line: Int, thread: Thread): Breakpoint
  def deleteBreakpoint(breakpoint: Breakpoint)
  def listBreakpoints(): List[Breakpoint]

  def query(variable: Variable): String
  
  def currentThread(): Thread
  def listThreads(): List[Thread]

  def backtrace(thread: Thread): Backtrace
}

class Ruab (
  val debugSymbolFile: String,
  val ecdbg: EcodeDebugger
  )
  extends TcodeDebugger
{
  val debugSymbols = DebugSymbols.load(debugSymbolFile)
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

