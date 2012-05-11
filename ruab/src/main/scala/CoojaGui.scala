import se.sics.cooja.Simulation
import se.sics.cooja.GUI

package ch.ethz.inf.vs.ruab {

class CoojaGui(
  tcdbg: TcodeDebugger,
  sourcefile: String,
  sim: Simulation,
  gui: GUI
  )
  extends Frontend
{
  def start(): Unit = ()
  def stop(): Unit = ()
}

}
