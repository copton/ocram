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
import java.io.IOException;

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
import java.util.HashMap;
import java.util.Map;

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
  private HashMap<Integer, Long> cycles;

  public OcramCoojaPlugin(Mote mote, Simulation sim, GUI gui) {
    super("Ocram Cooja Plugin", gui, false);
    simulation = sim;
    mspMote = (MspMote) mote;
    cpu = mspMote.getCPU();
    appStacks = new ArrayList<AppStack>();
    cycles = new HashMap<Integer, Long>();
  }

  public boolean setConfigXML(Collection<Element> configXML, boolean visAvailable) {
    appStacks.add(new AppStack("<<main>>"));

    for (Element element : configXML) {
      String name = element.getName();
      if ("appstack".equals(name)) {
        String variable = element.getText();
        if (variable.isEmpty()) {
          throw new OcramError("appstack without name");
        }
        int size;
        try {
          size = element.getAttribute("size").getIntValue();
        } catch (org.jdom.DataConversionException e) {
          throw new OcramError("appstack with non-integer size attribute", e);
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
    } catch (IOException e) {
        throw new OcramError("could not open logfile", e);
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
        throw new OcramError("failed to determine md5 sum of elf file", e);
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
          throw new OcramError("could not find stack variable: " + appstack.variable, e);
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
            throw new OcramError("Stack overflow! (stack = " + appStackVariable + ", PC=" + Integer.toHexString(cpu.reg[MSP430.PC]) + ", adr=" + Integer.toHexString(adr) + ", data=" + Integer.toHexString(data) + ")");
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
        throw new OcramError("failed to install cpu cycle monitor", e);
    }

    cpu.addWatchPoint(hookAddress, cycleMonitor = new CPUMonitor() {
      int currentAddress = 0;
      long enterCycles;
      public void cpuAction(int type, int adr, int data) {
        if (type == CPUMonitor.MEMORY_WRITE) {
          if (data == 0) {
            assert (currentAddress != 0);

            Long value = cycles.get(currentAddress);
            if (value == null) {
              value = new Long(0);
            }
            value += cpu.cycles - enterCycles;
            cycles.put(currentAddress, value);
            
            currentAddress = 0;
          }  else {
            currentAddress = data;
            enterCycles = cpu.cycles;
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
    for (Map.Entry<Integer, Long> entry : cycles.entrySet()) {
      log("cpu cycles: " + Integer.toHexString(entry.getKey()) + ": " + entry.getValue());
    }

    // shutdown
    try {
      logwriter.close();
    } catch (IOException e) {
      throw new OcramError("failed to close logfile", e); 
    }
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
        throw new OcramError("failed to write to log file", e);
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

  public class OcramError extends Error {
    public OcramError(String what) {
      this(what, null);
    }

    public OcramError(String what, Throwable cause) {
      super("Exception in Ocram-Cooja plugin: " + what);
      if (cause != null) {
        initCause(cause);
      }
    }
  }
}
