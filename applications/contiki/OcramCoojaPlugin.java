package se.sics.cooja.mspmote.plugins;


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

// logging
import org.apache.log4j.Logger;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;

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

  private BufferedWriter logwriter;

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

    // setup
    logger.info("loading Ocram Cooja Plugin...");
    try {
        File logfile = new File("OcramCooja.log");
        if (logfile.exists()) {
            logfile.delete();
        }
        logwriter = new BufferedWriter(new FileWriter(logfile));
    } catch (java.io.IOException e) {
        logger.error("failed to open log file: " + e);
        simulation.stopSimulation();
    }
    log("random seed: " + simulation.getRandomSeed());

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
                log("max stack size: " + maxStack);
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
            if (ev.msg.equals("QUIT")) {
                simulation.stopSimulation();
            } else {
                log("log output: " + ev.getTime() + ": " + ev.getMote().getID() + ": " + ev.msg);
            }
        }
    });


    // UI stuff
    setSize(50, 50);

    logger.info("Ocram Cooja Plugin loaded.");
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

  private void log(String logline) {
    logger.info(logline);

    try {
        logwriter.write(logline);
        logwriter.write("\n");
        logwriter.flush();
    } catch (java.io.IOException e) {
        logger.error("failed do write to log file: " + e);
        simulation.stopSimulation();
    }
  }
}
