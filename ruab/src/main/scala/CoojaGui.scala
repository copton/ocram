import se.sics.cooja.Simulation
import se.sics.cooja.GUI
import se.sics.cooja.VisPlugin
import javax.swing.JOptionPane

package ch.ethz.inf.vs.ruab {

class CoojaGui(
  tcdbg: TcodeDebugger,
  tcodefile: String,
  ecodefile: String,
  dbginfo: DebugInformation,
  cooja: VisPlugin
  )
  extends Frontend
{
  def start(): Unit = {
    val debuginfo = dbginfo.load()
    val tcode = Utils.readFileVerify(tcodefile, debuginfo.checksum(0)).getOrElse(throw new RuntimeException("checksum for %s failed" format tcodefile))
    val ecode = Utils.readFileVerify(ecodefile, debuginfo.checksum(1)).getOrElse(throw new RuntimeException("checksum for %s failed" format ecodefile))

    JOptionPane.showMessageDialog(cooja, "locked and loaded!")
  }

  def stop(): Unit = ()
}


}
