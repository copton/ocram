// simlink this file to
// contiki/tools/cooja/apps/mspsim/src/se/sics/cooja/mspmote/plugins/
//
// and build it using the cooja build system

package se.sics.cooja.mspmote.plugins;

//<1 imports
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
import java.util.HashSet;
import java.util.Map;
//>1

@ClassDescription("Ocram Cooja Plugin")
@PluginType(PluginType.MOTE_PLUGIN)
public class OcramCoojaPlugin extends VisPlugin implements MotePlugin {
  private static abstract class Monitor { //<1
    protected MspMote mote;
    protected MSP430 cpu;
    protected Simulation simulation;

    public Monitor(Simulation simulation, MspMote mote) {
      this.simulation = simulation;
      this.mote = mote;
      this.cpu = mote.getCPU();
    }
    public void configure(Collection<Element> config) { }
    public abstract void start();
    public void stop() { }
  }

  private static class StackMonitor extends Monitor {  //<1
    public StackMonitor(Simulation s, MspMote m) { 
      super(s, m);
      appStacks = new ArrayList<AppStack>();
    }

    private CPUMonitor stackMonitor = null;
    private Observer stackObserver = null;
    private ArrayList<AppStack> appStacks;
    private AppStack currentAppStack;

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
        return variable + "[" + size + "] (" + hex(lowerAddress) + ")";
      }
    }


    public void configure(Collection<Element> config) { 
      HashSet<String> appStackVars = new HashSet<String>();
      appStacks.add(new AppStack("<<main>>"));
      for (Element element : config) {
        String name = element.getName();
        if ("appstack".equals(name)) {
          String variable = element.getText();
          if (variable.isEmpty()) {
            throw new OcramError("appstack without name");
          }
          if (appStackVars.contains(variable)) {
            throw new OcramError("duplicate appstack: " + variable);
          }
          appStackVars.add(variable);
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
    }

    public void start() {  
      AddressMemory memory = (AddressMemory) mote.getMemory();
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
              throw new OcramError("Stack overflow! (stack = " + appStackVariable + ", PC=" + hex(cpu.reg[MSP430.PC]) + ", adr=" + hex(adr) + ", data=" + hex(data) + ")");
            }
          }
        };
        cpu.addWatchPoint(appstack.lowerAddress, appstack.monitor);
      }

      cpu.addRegisterWriteMonitor(MSP430.SP, stackMonitor = new CPUMonitor() {
        public void cpuAction(int type, int adr, int value) {
          // XXX nur schreibzugriff?
          /* check for context switch */
          for (AppStack appstack : appStacks) {
            if (value == appstack.lastSP) {
              assert (appstack != currentAppStack);
              currentAppStack = appstack;
              return;
            }
          }

          currentAppStack.lastSP = value;
          // XXX negative Stackpointer?
          int size = ((currentAppStack.upperAddress - value) + 0xffff) % 0xffff;
          if (currentAppStack.maxStack < size) {
            currentAppStack.maxStack = size;
          }
        }
      });
    }

    public void stop() { 
      mote.getStackOverflowObservable().deleteObserver(stackObserver);
      cpu.removeRegisterWriteMonitor(MSP430.SP, stackMonitor);
      for (AppStack appstack : appStacks) {
        if (appstack.monitor != null) {
          cpu.removeWatchPoint(appstack.lowerAddress, appstack.monitor);
          log("max stack: " + appstack.variable + ": " + appstack.maxStack);
        }
      }
    }
  }

  private static class LogMonitor extends Monitor { //<1
    private LogOutputListener logMonitor;

    public LogMonitor(Simulation s, MspMote m) {
      super(s, m);
    }

    public void start() {
      simulation.getEventCentral().addLogOutputListener(logMonitor = new LogOutputListener() {
        public void moteWasAdded(Mote mote) { }
        public void moteWasRemoved(Mote mote) { }
        public void removedLogOutput(LogOutputEvent ev) { }
        public void newLogOutput(LogOutputEvent ev) {
          log("log output: " + ev.getTime() + ": " + ev.getMote().getID() + ": " + ev.msg);
        }
      });
    }

    public void stop() {
      simulation.getEventCentral().removeLogOutputListener(logMonitor);
    }
  }

  private static class CycleMonitor extends Monitor {  //<1
    private CPUMonitor cycleMonitor;
    private int hookAddress;
    private ArrayList<String> processVariables;
    private HashMap<Integer, String> processes;
    private HashMap<Integer, Long> cycles;

    public CycleMonitor(Simulation s, MspMote m) { 
      super(s, m);

      processVariables = new ArrayList<String>();
      processes = new HashMap<Integer, String>();
      cycles = new HashMap<Integer, Long>();
    }

    public void configure(Collection<Element> config) { 
      HashSet<String> processVars = new HashSet<String>();
      for (Element element : config) {
        String name = element.getName();
        if ("process".equals(name)) {
          String variable = element.getText();
          if (variable.isEmpty()) {
            throw new OcramError("process without name");
          }
          if (processVars.contains(variable)) {
            throw new OcramError("dublicate process: " + variable);
          }
          processVars.add(variable);
          processVariables.add(variable);
        }
      }
    }

    public void start() { 
      AddressMemory memory = (AddressMemory) mote.getMemory();
      try {
        hookAddress = memory.getVariableAddress("process_hook");
      } catch (UnknownVariableException e) {
        throw new OcramError("failed to install cpu cycle monitor", e);
      }

      for (String variable: processVariables) {
        int address;
        try {
          address = memory.getVariableAddress(variable);
        } catch (UnknownVariableException e) {
          throw new OcramError("could not find process variable: " + variable, e);
        }
        log("process monitoring: " + variable);
        processes.put(address, variable);
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
    }

    public void stop() { 
      // cpu cycle monitor
      cpu.removeWatchPoint(hookAddress, cycleMonitor);
      for (Map.Entry<Integer, Long> entry : cycles.entrySet()) {
        String identifier;
        if (processes.containsKey(entry.getKey())) {
          identifier = processes.get(entry.getKey());
        } else {
          identifier = hex(entry.getKey());
        }
        log("cpu cycles: " + identifier + ": " + entry.getValue());
      }
    }
  }

  private static class DebugMonitor extends Monitor { //<1
    private int markAddress;
    private CPUMonitor markMonitor;

    public DebugMonitor(Simulation s, MspMote m) { 
      super(s, m);
    }

    public void start() {
      AddressMemory memory = (AddressMemory) mote.getMemory();
      try {
        markAddress = memory.getVariableAddress("debug_mark");
        memory.getIntValueOf("debug_file"); // just to check existence
        memory.getIntValueOf("debug_line"); // just to check existence
        cpu.addWatchPoint(markAddress, markMonitor = new CPUMonitor() {
          public void cpuAction(int type, int adr, int data) {
            if (type == CPUMonitor.MEMORY_WRITE) {
              if (data == 0xffff) {
                int fileAddress = ((AddressMemory)mote.getMemory()).getIntValueOf("debug_file");
                byte bytes[] = mote.getMemory().getMemorySegment(fileAddress, 20);
                int i;
                for (i=0; bytes[i] != 0 && i<bytes.length; i++);
                String filename = new String(bytes, 0, i);
                int line = ((AddressMemory)mote.getMemory()).getIntValueOf("debug_line");
                String logline = "Assertion error: file=" + filename + ", line=" + line;
                log(logline);
                throw new OcramError(logline);
              } else {
                long simtime = simulation.getSimulationTimeMillis();
                long systime = System.currentTimeMillis();
                log("debug monitor: simulation time=" + simtime + ", system time=" + systime + ": mark: " + data);
              }
            }
          }
        });
      } catch (UnknownVariableException e) {
        throw new OcramError("failed to find debug variables", e);
      }
    }

    public void stop() { 
      if (markMonitor != null) {
        cpu.removeWatchPoint(markAddress, markMonitor);
      }   
    }
  }

  //<1 OcramCooja
  private ArrayList<Monitor> monitors;
  private MspMote mote;
  private Simulation simulation;

  public OcramCoojaPlugin(Mote mote_, Simulation sim, GUI gui) { 
    super("Ocram Cooja Plugin", gui, false);
    simulation = sim;
    mote = (MspMote) mote_;

    monitors = new ArrayList<Monitor>();
    monitors.add(new StackMonitor(simulation, mote));
    monitors.add(new LogMonitor(simulation, mote));
    monitors.add(new CycleMonitor(simulation, mote));
    monitors.add(new DebugMonitor(simulation, mote));
  }

  public boolean setConfigXML(Collection<Element> configXML, boolean visAvailable) { //<2
    for (Monitor monitor: monitors) {
      monitor.configure(configXML);
    }
    return true;
  }

  public void startPlugin() { //<2
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
      File firmware = ((MspMoteType)mote.getType()).getContikiFirmwareFile();
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

    for (Monitor monitor : monitors) {
      monitor.start();
    }

    logger.info("Ocram Cooja Plugin loaded.");
  }

  public void closePlugin() { //<2
    for (Monitor monitor : monitors) {
      monitor.stop();
    }

    // shutdown
    try {
      logwriter.close();
    } catch (IOException e) {
      throw new OcramError("failed to close logfile", e); 
    }
  }

  public Mote getMote() { //<2
    return mote;
  }

  //<2 logging
  private static Logger logger = Logger.getLogger(OcramCoojaPlugin.class);
  private static BufferedWriter logwriter;

  private static void log(String logline) {
    logger.info(logline);

    try {
      logwriter.write(logline);
      logwriter.write("\n");
      logwriter.flush();
    } catch (java.io.IOException e) {
      throw new OcramError("failed to write to log file", e);
    }
  }

  //<2 OcramError
  public static class OcramError extends RuntimeException {
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

  //<2 utilities
  private static String hex(Integer value)
  {
    return "0x" + Integer.toHexString(value);
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
