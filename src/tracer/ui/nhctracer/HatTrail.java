package nhctracer;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.Properties;
import java.lang.Integer;

public class HatTrail {
  
  static TraceFrame traceFrame;
  
  static void usage() {
    System.err.println("Usage: java HatTrail port-number [program-name]");
    System.exit(1);
  }
  
  public static void main(String[] argv) {
    int portno = 6710;
    try {
      Integer p = new Integer(argv[0]);
      portno = p.intValue();
    } catch (Exception e) {
      usage();
    }
    traceFrame = new TraceFrame("localhost",portno);
    traceFrame.setSize(600, 700);
    traceFrame.setVisible(true);
    switch (argv.length) {
    case 1:
      break;
    case 2:
      traceFrame.connectToTrace(argv[1],"");
      break;
    case 4:
	if (argv[2].equals("-remote")) {
	    traceFrame.connectToTrace(argv[1],argv[3]);
	    break;
	} // else proceed as default
    default:
      usage();
    }
  }
}

