package se.sics.cooja.mspmote.plugins;

import org.apache.log4j.Logger;

import se.sics.cooja.ClassDescription;
import se.sics.cooja.GUI;
import se.sics.cooja.Mote;
import se.sics.cooja.MotePlugin;
import se.sics.cooja.PluginType;
import se.sics.cooja.Simulation;
import se.sics.cooja.VisPlugin;
import se.sics.cooja.mspmote.MspMote;
import se.sics.mspsim.core.MSP430;
import se.sics.mspsim.util.Utils;

// stack monitor
import se.sics.mspsim.core.CPUMonitor;
import se.sics.mspsim.util.MapTable;
import java.util.Observable;
import java.util.Observer;

// log monitor
import se.sics.cooja.SimEventCentral.LogOutputListener; 
import se.sics.cooja.SimEventCentral.LogOutputEvent;

@ClassDescription("Ocram Cooja Plugin")
@PluginType(PluginType.MOTE_PLUGIN)
public class OcramCoojaPlugin extends VisPlugin implements MotePlugin {
  private static Logger logger = Logger.getLogger(OcramCoojaPlugin.class);
  
  private MspMote mspMote;
  private MSP430 cpu;
  private Simulation simulation;

  // stack monitor
  private CPUMonitor cpuMonitor = null;
  private Observer stackObserver = null;
  private int maxStack;
  private int stackStartAddress = 0xa00;

  // log monitor
  private LogOutputListener logMonitor = null;

  public OcramCoojaPlugin(Mote mote, Simulation sim, GUI gui) {
    super("Ocram Cooja Plugin", gui);
    simulation = sim;
    mspMote = (MspMote) mote;
    cpu = mspMote.getCPU();


    // stack monitor
    maxStack = 0;

    if (cpu.getDisAsm() != null) {                         
        MapTable mapTable = cpu.getDisAsm().getMap();        
        if (mapTable != null) {                              
            stackStartAddress = mapTable.stackStartAddress;   
        }                                                    
    }                                                      

    cpu.setRegisterWriteMonitor(MSP430.SP, cpuMonitor = new CPUMonitor() {
        public void cpuAction(int type, int adr, int data) {
            int size = ((stackStartAddress - data) + 0xffff) % 0xffff;
            if (maxStack < size) {
                maxStack = size;
                logger.info("max stack size: " + maxStack);
            }
        }
    });

    mspMote.getStackOverflowObservable().addObserver(stackObserver = new Observer() {
        public void update(Observable obs, Object obj) {
            simulation.stopSimulation();
            logger.error(
                "Bad memory access!\nSimulation stopped.\n" +
                "\nCurrent stack pointer = 0x" + Utils.hex16(cpu.reg[MSP430.SP]) +
                "\nStart of heap = 0x" + Utils.hex16(cpu.getDisAsm().getMap().heapStartAddress)
                );
        }
    });
    mspMote.monitorStack(true);

    // log  monitor
    simulation.getEventCentral().addLogOutputListener(logMonitor = new LogOutputListener() {
        public void moteWasAdded(Mote mote) { }
        public void moteWasRemoved(Mote mote) { }
        public void removedLogOutput(LogOutputEvent ev) { }
        public void newLogOutput(LogOutputEvent ev) {
            logger.info("log output: " + ev.getMote().getID() + ": " + ev.getTime() + ": " + ev.msg);
        }
    });


    // UI stuff
    setSize(50, 50);
  }

  public void closePlugin() {
    // stack monitor
    mspMote.getStackOverflowObservable().deleteObserver(stackObserver);
    cpu.setRegisterWriteMonitor(MSP430.SP, null);

    // log monitor
    simulation.getEventCentral().removeLogOutputListener(logMonitor);
  }

  public Mote getMote() {
    return mspMote;
  }

}
