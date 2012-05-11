import java.util.Collection
import org.jdom.Element
import scala.collection.JavaConversions._
import se.sics.cooja.ClassDescription
import se.sics.cooja.GUI
import se.sics.cooja.Mote
import se.sics.cooja.MotePlugin
import se.sics.cooja.PluginType
import se.sics.cooja.Simulation
import se.sics.cooja.VisPlugin

package ch.ethz.inf.vs.ruab {

@ClassDescription("Ruab Cooja Plugin")
@PluginType(PluginType.MOTE_PLUGIN)
class CoojaPlugin(mote: Mote, sim: Simulation, gui: GUI)
  extends VisPlugin("Ruab plugin", gui, false)
  with MotePlugin
{
  var frontend: Frontend = null

  override def setConfigXML(config: Collection[Element], visAvailable: Boolean): Boolean = {
    var sourcefile = ""
    var debugfile = ""
    var frontend = ""
    for (element <- config) {
      element.getName match {
        case "sourcefile" => sourcefile = element.getText
        case "debugfile" => debugfile = element.getText
        case "frontend" => frontend = element.getText
      }
    }
    for (item <- List(sourcefile, debugfile, frontend)) {
      if (item.isEmpty) {
        throw new RuntimeException("no %s config found" format item)
      }
    }
    val ecdbg = new CoojaBackend(mote)
    val tcdbg = new Ruab(debugfile, ecdbg)
    this.frontend = frontend match {
      case "cooja" => new CoojaGui(tcdbg, sourcefile, sim, gui)
      case x => throw new RuntimeException("unknown frontend %s" format x)
    }
    true
  }

  override def startPlugin(): Unit = 
    frontend.start()

  override def closePlugin(): Unit =
    frontend.stop()

  override def getMote(): Mote = mote
}

}
