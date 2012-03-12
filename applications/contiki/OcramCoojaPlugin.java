// simlink this file to
// contiki/tools/cooja/apps/mspsim/src/se/sics/cooja/mspmote/plugins/
//
// and build it using the cooja build system
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
import java.io.FileInputStream;
import java.io.FileWriter;
import se.sics.cooja.mspmote.MspMoteType;
import java.security.MessageDigest;

// stack monitor
import se.sics.mspsim.core.CPUMonitor;
import se.sics.mspsim.util.MapTable;
import java.util.Observable;
import java.util.Observer;

// log monitor
import se.sics.cooja.SimEventCentral.LogOutputListener; 
import se.sics.cooja.SimEventCentral.LogOutputEvent;

// cpu cycle monitor
import se.sics.cooja.AddressMemory;
import se.sics.cooja.AddressMemory.UnknownVariableException;

@ClassDescription("Ocram Cooja Plugin")
@PluginType(PluginType.MOTE_PLUGIN)
public class OcramCoojaPlugin extends VisPlugin implements MotePlugin {
  private static Logger logger = Logger.getLogger(OcramCoojaPlugin.class);
  
  private MspMote mspMote;
  private MSP430 cpu;
  private Simulation simulation;

  private BufferedWriter logwriter;

  // stack monitor
  private CPUMonitor stackMonitor = null;
  private Observer stackObserver = null;
  private int maxStack;
  private int stackStartAddress = 0xa00;

  // log monitor
  private LogOutputListener logMonitor = null;
  
  // cycle monitor
  private CPUMonitor cycleMonitor = null;
  private int address;

  public OcramCoojaPlugin(Mote mote, Simulation sim, GUI gui) {
    super("Ocram Cooja Plugin", gui, false);
    simulation = sim;
    mspMote = (MspMote) mote;
    cpu = mspMote.getCPU();
    File firmware = ((MspMoteType)mspMote.getType()).getContikiFirmwareFile();

    // setup
    logger.info("loading Ocram Cooja Plugin...");
    try {
        File logfile = new File("OcramCooja.log");
        if (logfile.exists()) {
            logfile.delete();
        }
        logwriter = new BufferedWriter(new FileWriter(logfile));
        log("logfile open: " + logfile.getCanonicalPath());
    } catch (java.io.IOException e) {
        logger.error("failed to open log file: " + e);
        simulation.stopSimulation();
        return;
    }

    // open log
    String path;
    String checksum;
    try {
        byte[] contents = new byte[(int)firmware.length()];
        FileInputStream input = new FileInputStream(firmware);
        input.read(contents);
        path = firmware.getCanonicalPath();
        MessageDigest md = MessageDigest.getInstance("MD5");
        md.update(contents);
        checksum = getHex(md.digest());
    } catch (Exception e) {
        logger.error("failed to determine md5 sum of elf file: " + e);
        simulation.stopSimulation();
        return;
    }
    log("md5 sum of " + path  + ": " + checksum);
    log("random seed: " + simulation.getRandomSeed());

    // stack monitor
    maxStack = 0;

    if (cpu.getDisAsm() != null) {                         
        MapTable mapTable = cpu.getDisAsm().getMap();        
        if (mapTable != null) {                              
            stackStartAddress = mapTable.stackStartAddress;   
        }                                                    
    }                                                      

    cpu.setRegisterWriteMonitor(MSP430.SP, stackMonitor = new CPUMonitor() {
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
            log("log output: " + ev.getTime() + ": " + ev.getMote().getID() + ": " + ev.msg);
        }
    });

    // cpu cycle monitor
    AddressMemory memory = (AddressMemory) mote.getMemory();
    try {
        address = memory.getVariableAddress("process_hook");
    } catch (UnknownVariableException e) {
        simulation.stopSimulation();
        logger.error("faild to install cpu cycle monitor: " + e);
        return;
    }

    cpu.setBreakPoint(address, cycleMonitor = new CPUMonitor() {
        int current_adr = 0;
        long enter_cycles;
        public void cpuAction(int type, int adr, int data) {
            if (type == CPUMonitor.MEMORY_WRITE) {
                if (data == 0) {
                    assert (current_adr != 0);
                    log("cycle monitor action: 0x" + Integer.toHexString(current_adr) + ", " + (cpu.cycles - enter_cycles));
                    current_adr = 0;
                }  else {
                    current_adr = data;
                    enter_cycles = cpu.cycles;
                }
            }
        }
    });

    logger.info("Ocram Cooja Plugin loaded.");
  }

  public void closePlugin() {
    // stack monitor
    mspMote.getStackOverflowObservable().deleteObserver(stackObserver);
    cpu.setRegisterWriteMonitor(MSP430.SP, null);

    // log monitor
    simulation.getEventCentral().removeLogOutputListener(logMonitor);

    // cpu cycle monitor
    cpu.clearBreakPoint(address);
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

  private static final String HEXES = "0123456789abcdef";
  private static String getHex( byte [] raw ) {
    if ( raw == null ) {
      return null;
    }
    final StringBuilder hex = new StringBuilder( 2 * raw.length );
    for ( final byte b : raw ) {
      hex.append(HEXES.charAt((b & 0xF0) >> 4))
         .append(HEXES.charAt((b & 0x0F)));
    }
    return hex.toString();
  }
}
