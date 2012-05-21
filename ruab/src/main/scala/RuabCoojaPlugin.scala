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
import org.apache.log4j.Logger;

package ch.ethz.inf.vs.ruab {

@ClassDescription("Ruab Cooja Plugin")
@PluginType(PluginType.MOTE_PLUGIN)
class RuabCoojaPlugin(mote: Mote, sim: Simulation, gui: GUI)
  extends VisPlugin("Ruab plugin", gui, false)
  with MotePlugin
{
  var frontend: Frontend = null

  val logger = Logger.getLogger(classOf[RuabCoojaPlugin])

  override def setConfigXML(config: Collection[Element], visAvailable: Boolean): Boolean = {
    logger.info("configuring Ruab plugin")
    var tcodefile = ""
    var ecodefile = ""
    var debugfile = ""
    for (element <- config) {
      element.getName match {
        case "tcodefile" => tcodefile = element.getText
        case "ecodefile" => ecodefile = element.getText
        case "debugfile" => debugfile = element.getText
      }
    }
    for (item <- List(tcodefile, ecodefile, debugfile)) {
      if (item.isEmpty) {
        throw new RuntimeException("no %s config found" format item)
      }
    }
    val ecdbg = new CoojaBackend(mote)
    val dbginfo = new OcramDebugInformation(debugfile)
    val tcdbg = new Ruab(dbginfo, ecdbg)
    frontend = new CoojaGui(tcdbg, tcodefile, ecodefile, dbginfo, this)
    true
  }

  override def startPlugin(): Unit = {
    logger.info("starting Ruab plugin")
    if (frontend == null) {
      throw new RuntimeException("need config XML for this plugin")
    } else frontend.start()
  }


  override def closePlugin(): Unit = {
    logger.info("stopping Ruab plugin")
    if (frontend != null)
      frontend.stop()
    }

  override def getMote(): Mote = mote
}

}
