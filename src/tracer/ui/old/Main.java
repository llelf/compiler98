package nhctracer;

import java.awt.*;
import java.applet.*;

public class Main extends Applet {
  DbgPanel dbgPanel;
  OutputPanel outputPanel;
  SourceViewer viewer;
  Status status;
  TabbedPanel tabPanel;
  Button connect;
  Button disconnect;
  Connection serverConnection;
  String host;
  int port;

  public void init() {
    status = new Status("Not connected");
    viewer = new SourceViewer(true, status);
    dbgPanel = new DbgPanel(viewer, status);
    outputPanel = new OutputPanel(status);
    outputPanel.setDbgPanel(dbgPanel);
    connect = new Button("Connect");
    disconnect = new Button("Disconnect");

    Label dbgLabel = new Label("Trace window", Label.CENTER);
    dbgLabel.setBackground(Color.red);
    dbgLabel.setForeground(Color.white);
    dbgLabel.setFont(new Font("Helvetica", Font.PLAIN, 20));
        
    Panel pb = new Panel();
    pb.setLayout(new FlowLayout());
    pb.add(connect);
    pb.add(disconnect);

    TabbedPanel tp = new TabbedPanel();
    tp.addItem("Trace browser", dbgPanel);

     Panel p1 = new Panel();
     p1.setLayout(new BorderLayout());
     //     p1.add("North", dbgLabel);
     p1.add("Center", tp);
     p1.add("South", status);

    tabPanel = new TabbedPanel();
    tabPanel.addItem("Program output", outputPanel);
    tabPanel.addItem("Source code", viewer);
    viewer.setTabPanel(tabPanel);
    outputPanel.setTabPanel(tabPanel);

    //    p2.setLayout(new BorderLayout());
    //    p2.add("North", outputLabel);
    //    p2.add("South", outputPanel);


    this.setLayout(new BorderLayout());
    this.add("North", pb);
    this.add("Center", p1);
    this.add("South", tabPanel);

    host = getParameter("host");
    try { port = Integer.parseInt(getParameter("port")); }
    catch (NumberFormatException e) { port = 6705; }
    //    host = "indy105.cs.york.ac.uk";
    //    port = 6705;
  }

  public void stop() {
    if (serverConnection != null) {
      outputPanel.disconnected();      
      serverConnection = null;
      viewer.reset();
    }
  }

  public boolean handleEvent(Event evt) {
    switch (evt.id) {
    case Event.ACTION_EVENT:
      if (evt.target == connect) {
	if (serverConnection == null) {
	  dbgPanel.status.setText("Connecting to server. Please wait.");
	  serverConnection = new Connection(host, port);
	  if (serverConnection.error == null) {
	    outputPanel.connected(serverConnection);      
	  } else {
	    dbgPanel.status.setText(serverConnection.error);
	    serverConnection = null;
	  }
	} else {
	  dbgPanel.status.setText("Already connected");
	}
	return true;
      } else if (evt.target == disconnect) {
	if (serverConnection == null) {
	  dbgPanel.status.setText("Not connected");
	} else {
	  outputPanel.disconnected();      
	  serverConnection = null;
	  viewer.reset();
	}
	return true;
      }
    }
    return super.handleEvent(evt);
  }
}

