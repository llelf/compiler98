package nhctracer;

import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.util.Vector;
import java.io.*;

public class DemoPanel extends Panel {
  Connection serverConnection;
  public final static int MAX_EXAMPLES = 32;
  MainPanel mainPanel;
  TextField tf = new TextField();
  Applet applet;

  public DemoPanel(Applet _applet) {
    applet = _applet;
    
    String[] labels = new String[MAX_EXAMPLES];
    int[] ports = new int[MAX_EXAMPLES];
    String[] messages = new String[MAX_EXAMPLES];

    String s;
    Panel buttons = new Panel(new FlowLayout());

    mainPanel = new MainPanel(null);

    for (int i = 0; (s = applet.getParameter("port" + (i+1))) != null; i++) {
      //System.err.println("Adding number " + (i+1));
      Button b = new Button(applet.getParameter("label" + (i+1)));
      buttons.add(b);
      b.addActionListener(new ExampleStarter(this,
					     Integer.parseInt(s),
					     applet.getParameter("message" + 
								 (i+1))));
    }
    
    Button b = new Button("Reset");
    buttons.add(b);
    b.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
	mainPanel.outputPanel.disconnected();      
	mainPanel.viewer.reset();	
	tf.setText("");
      }    
    });

    Panel topPanel = new Panel(new BorderLayout());

    topPanel.add(buttons, BorderLayout.NORTH);
    topPanel.add(tf, BorderLayout.SOUTH);

    setLayout(new BorderLayout());
    add(topPanel, BorderLayout.NORTH);
    add(mainPanel, BorderLayout.CENTER);
  }

  public void stop() {
    //System.err.println("Stopping");
    mainPanel.outputPanel.disconnected();      
    mainPanel.viewer.reset();
  }

  public void start() {
    //System.err.println("Starting");	  
    repaint();
  }
}

class ExampleStarter implements ActionListener {
  int port;
  String message;
  DemoPanel demoPanel;

  public ExampleStarter(DemoPanel demoPanel, int port, String message) {
    this.demoPanel = demoPanel;
    this.port = port;
    this.message = message;
  }
  
  public void actionPerformed(ActionEvent evt) {
    if (demoPanel.serverConnection != null) {
      demoPanel.mainPanel.outputPanel.disconnected();      
      demoPanel.mainPanel.viewer.reset();
    }	  
    demoPanel.mainPanel.status.setText("Connecting to server. Please wait.");
    demoPanel.serverConnection = new Connection(demoPanel.applet.getCodeBase().getHost(), port);
    if (demoPanel.serverConnection.error == null) {
      demoPanel.mainPanel.outputPanel.connected(demoPanel.serverConnection);
      demoPanel.tf.setText(message);
    } else {
      demoPanel.mainPanel.status.setText(demoPanel.serverConnection.error);
      demoPanel.serverConnection = null;
    }
  }
}
