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
import java.util.ArrayList;
import java.util.Collection;
import org.jdom.Element;

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
  private class AppStack {
    public String variable;
    public int size;
    public int upperAddress;
    public int lowerAddress;
    public CPUMonitor monitor;
    public int maxStack;
    public int lastSP;
    public AppStack(String v) {
      variable = v;
      upperAddress = -1;
      lowerAddress = -1;
      size = -1;
      monitor = null;
      maxStack = -1;
      lastSP = -1;
    }
    public void init(int low, int high) {
      lowerAddress = low;
      upperAddress = high;
      size = upperAddress - lowerAddress;
      maxStack = 0;
      lastSP = upperAddress - 2; // our monitor get called _after_ the first push operation
    }
    public String toString() {
      return variable + "[" + size + "] (0x" + Integer.toHexString(lowerAddress) + ")";
    }
  }
  private ArrayList<AppStack> appStacks;
  AppStack currentAppStack;

  // log monitor
  private LogOutputListener logMonitor = null;
  
  // cycle monitor
  private CPUMonitor cycleMonitor = null;
  private int hookAddress;

  public OcramCoojaPlugin(Mote mote, Simulation sim, GUI gui) {
    super("Ocram Cooja Plugin", gui, false);
    simulation = sim;
    mspMote = (MspMote) mote;
    cpu = mspMote.getCPU();
    appStacks = new ArrayList<AppStack>();
  }

  public boolean setConfigXML(Collection<Element> configXML, boolean visAvailable) {
    appStacks.add(new AppStack("<<main>>"));

    for (Element element : configXML) {
      String name = element.getName();
      if ("appstack".equals(name)) {
        String variable = element.getText();
        assert (! variable.isEmpty());
        int size;
        try {
          size = element.getAttribute("size").getIntValue();
        } catch (org.jdom.DataConversionException e) {
          AssertionError e2 = new AssertionError();
          e2.initCause(e);
          throw e2;
        }

        AppStack appStack = new AppStack(variable);
        appStack.size = size;
        appStacks.add(appStack);
      }
    }
    return true;
  }

  public void startPlugin() {
    AddressMemory memory = (AddressMemory) mspMote.getMemory();
    File firmware = ((MspMoteType)mspMote.getType()).getContikiFirmwareFile();

    // setup
    logger.info("starting Ocram Cooja Plugin...");
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
        logger.fatal("failed to determine md5 sum of elf file: " + e);
        simulation.stopSimulation();
        return;
    }
    log("md5 sum of " + path  + ": " + checksum);
    log("random seed: " + simulation.getRandomSeed());

    // stack monitor
    for (AppStack appstack : appStacks) {
      if (appstack.variable == "<<main>>") {
        MapTable map = cpu.getDisAsm().getMap();
        currentAppStack = appstack;
        appstack.init(map.heapStartAddress + 2, map.stackStartAddress);
      } else {
        int address;
        try {
          address = memory.getVariableAddress(appstack.variable);
        } catch (UnknownVariableException e) {
          logger.fatal("could not find stack variable: " + e);
          simulation.stopSimulation();
          return;
        }
        appstack.init(address, address + appstack.size);
      }
      log("application stack: " + appstack);

      final String appStackVariable = appstack.variable;
      appstack.monitor = new CPUMonitor() {
        private int count = 0;
        public void cpuAction(int type, int adr, int data) {
          if (type == CPUMonitor.MEMORY_WRITE) {
            // first access is by reset vector
            if (appStackVariable != "<<main>>" && count == 0) {
              count++;
              return;
            }
            logger.fatal("Stack overflow! (stack = " + appStackVariable + ", PC=" + Integer.toHexString(cpu.reg[MSP430.PC]) + ", adr=" + Integer.toHexString(adr) + ", data=" + Integer.toHexString(data) + ")\n");
            simulation.stopSimulation();
          }
        }
      };
      cpu.addWatchPoint(appstack.lowerAddress, appstack.monitor);
    }

    cpu.addRegisterWriteMonitor(MSP430.SP, stackMonitor = new CPUMonitor() {
        public void cpuAction(int type, int adr, int value) {
          /* check for context switch */
          for (AppStack appstack : appStacks) {
            if (value == appstack.lastSP) {
              assert (appstack != currentAppStack);
              currentAppStack = appstack;
              return;
            }
          }

          currentAppStack.lastSP = value;
          int size = ((currentAppStack.upperAddress - value) + 0xffff) % 0xffff;
          if (currentAppStack.maxStack < size) {
            currentAppStack.maxStack = size;
          }
        }
    });

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
    try {
        hookAddress = memory.getVariableAddress("process_hook");
    } catch (UnknownVariableException e) {
        simulation.stopSimulation();
        logger.error("failed to install cpu cycle monitor: " + e);
        return;
    }

    cpu.addWatchPoint(hookAddress, cycleMonitor = new CPUMonitor() {
        int current_adr = 0;
        long enter_cycles;
        public void cpuAction(int type, int adr, int data) {
            if (type == CPUMonitor.MEMORY_WRITE) {
                if (data == 0) {
                    assert (current_adr != 0);
// XXX                    log("cycle monitor action: 0x" + Integer.toHexString(current_adr) + ", " + (cpu.cycles - enter_cycles));
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
    cpu.removeRegisterWriteMonitor(MSP430.SP, stackMonitor);
    for (AppStack appstack : appStacks) {
      if (appstack.monitor != null) {
        cpu.removeWatchPoint(appstack.lowerAddress, appstack.monitor);
        log("max stack: " + appstack.variable + ": " + appstack.maxStack);
      }
    }

    // log monitor
    simulation.getEventCentral().removeLogOutputListener(logMonitor);

    // cpu cycle monitor
    cpu.removeWatchPoint(hookAddress, cycleMonitor);
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
