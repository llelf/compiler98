package nhctracer;

import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.io.*;
import java.util.Properties;
//import com.sun.java.swing.*;

public class NhcTracer extends Applet {
  static final String defaultHost = "localhost";
  static final int defaultPort = 6710;
  boolean isApplet = true;
  DemoPanel demoPanel;
  
  public void run() {
  }
  
  public void init() {
    String host, portStr;

    if (isApplet)
      GetParams.setApplet(this);

    if (isApplet)
      host = getParameter("nhctracer.host");
    else
      host = System.getProperty("nhctracer.host");

    if (host == null)
      host = defaultHost;
    if (isApplet)
      portStr = getParameter("nhctracer.port");
    else
      portStr = System.getProperty("nhctracer.port");
    int port = defaultPort;
    if (portStr != null) {
      try {
	port = Integer.parseInt(portStr);
      } catch (NumberFormatException e) {
	System.err.println("Bad port number " + portStr + ", using default.");
      }
    }
    if (isApplet) {
      demoPanel = new DemoPanel(this);
      add(demoPanel);
    } else {
      TraceFrame traceFrame = new TraceFrame(host,
					     port,
					     "Haskell Redex Trail Browser", 
					     "Jan Sparud",
					     "Version -0.01 alpha");
      traceFrame.setSize(600, 700);
      traceFrame.setVisible(true);
      String script = System.getProperty("nhctracer.script");
      if (script != null) {
	  traceFrame.runScript(script);
      }
    }
  }
  
  public void stop() {
    if (isApplet) {
      //demoPanel.stop();
      //System.err.println("After demoPanel.stop()");
      super.stop();
    }
  }

  public void start() {
    if (isApplet) {
      demoPanel.start();
    }
  }
	
  static void usage() {
    System.err.println("Usage: java NhcTracer port");
    System.exit(1);
  }
  
  public static void main(String[] argv) {
    /*
    int port = 0;
    String host = null;

    if (argv.length < 1)
      usage();
    try { port = Integer.parseInt(argv[0]); }
    catch (NumberFormatException e) { 
      usage();
    }
    if (argv.length == 2)
      host = argv[1];

    Properties p = System.getProperties();
    System.out.println("p = " + p + "\nhost = " + host + "\nport = " + port);
    p.put("nhctracer.host", host);
    //p.put("nhctracer.port", ""+port);
    //System.setProperties(p);
    */
    NhcTracer nt = new NhcTracer();
    nt.isApplet = false;
    nt.init();
    nt.start();
  }
}

