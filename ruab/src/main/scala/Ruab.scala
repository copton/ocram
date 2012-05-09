import java.util.Collection
import org.jdom.Element
import se.sics.cooja.ClassDescription
import se.sics.cooja.GUI
import se.sics.cooja.Mote
import se.sics.cooja.MotePlugin
import se.sics.cooja.PluginType
import se.sics.cooja.Simulation
import se.sics.cooja.VisPlugin

package ch.ethz.inf.vs.ocram {

@ClassDescription("Ruab Cooja Plugin")
@PluginType(PluginType.MOTE_PLUGIN)
class Ruab(mote: Mote, sim: Simulation, gui:GUI)
  extends VisPlugin("Ruab plugin", gui, false)
  with MotePlugin
{
  override def setConfigXML(configXML: Collection[Element], visAvailable: Boolean): Boolean =
    true

  override def startPlugin(): Unit = ()

  override def closePlugin(): Unit = ()

  override def getMote(): Mote = mote
}

}
