package nhctracer;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.Properties;

public class HatTrail {
  static void usage() {
    System.err.println("Usage: java HatTrail [program-name]");
    System.exit(1);
  }
  
  public static void main(String[] argv) {
    TraceFrame traceFrame = new TraceFrame("localhost",6710);
    traceFrame.setSize(600, 700);
    traceFrame.setVisible(true);
    switch (argv.length) {
    case 0:
      break;
    case 1:
      traceFrame.connectToTrace(argv[0]);
      break;
    default:
      usage();
    }
  }
}

