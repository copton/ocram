package ch.ethz.inf.vs.ruab {

object Types {
  type SourceLine = Int

  abstract class FQI( // fully-qualified identifier
    val name: String
  )
  case class GlobalIdentifier(override val name: String) extends FQI(name)
  case class ScopedIdentifier(
    override val name: String,
    val function: String,
    val scope: List[Int]
  ) extends FQI(name)


  class Frame(val function: String, val line: Int)
  class Backtrace(val frames: List[Frame])
}

trait Frontend {
  def start(): Unit
  def stop(): Unit
}

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

case class Coord(val col: Int, val row: Int)

abstract class Text(val text: String) {
  def coord2pos(coord: Coord): Int
  def eol(row: Int): Int
  def pos2coord(pos: Int): Coord
}

trait DebugInformation {
/*
  case class Variable(
    val tcodeInstance: Types.FQI,
    val ecodeName: Types.FQI,
    val critical: Boolean
  )
*/

  case class TLocation (
    val row: Int,
    val col: Int,
    val len: Int
  )

  case class ELocation (
    val row: Int,
    val col: Int,
    val tid: Option[Int]
  )

  case class Location (
    val start: Int,
    val end: Int,
    val tloc: TLocation,
    val eloc: ELocation
  )

/*
  case class Thread (
    val id: Int,
    val threadStartFunction: String,
    val callGraph: CallGraph
  )
*/
  def tcode(): Text
  def ptcode(): Text
  def ecode(): Text

  def trow2ptrow(row: Int): Int
  def getLocation(coord: Coord): Option[Location]
}

}
