package nhctracer;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.Properties;

public class HatTrail {
  static final String defaultHost = "localhost";
  static final int defaultPort = 6710;
  
  static void usage() {
    System.err.println("Usage: java HatTrack port");
    System.exit(1);
  }
  
  public static void main(String[] argv) {
    String host, portStr;
    int port;
    host = System.getProperty("nhctracer.host");
    if (host == null) host = defaultHost;
    portStr = System.getProperty("nhctracer.port");
    if (portStr == null) {
      port = defaultPort;
    } else {
      try {
	port = Integer.parseInt(portStr);
      } catch (NumberFormatException e) {
	System.err.println("Bad port number " + portStr + ", using default.");
	port = defaultPort;
      }
    }
    TraceFrame traceFrame = new TraceFrame(host,port);
    traceFrame.setSize(600, 700);
    traceFrame.setVisible(true);
    String script = System.getProperty("nhctracer.script");
    if (script != null) traceFrame.runScript(script);
  }
}

