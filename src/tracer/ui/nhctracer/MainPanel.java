package nhctracer;

import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.util.Vector;
import java.io.*;

public class MainPanel extends Panel {
  DbgPanel dbgPanel;
  OutputPanel outputPanel;
  SourceViewer viewer;
  Status status;
  TabbedPanel tabPanel;
  TraceFrame frame;

  public MainPanel(TraceFrame _frame) {
    frame = _frame;
    status = new Status("Not connected");
    viewer = new SourceViewer(true, status);
    dbgPanel = new DbgPanel(frame, this);
    outputPanel = new OutputPanel(frame, status);
    outputPanel.setDbgPanel(dbgPanel);
        
    TabbedPanel tp = new TabbedPanel();
    tp.addItem("Trace browser", dbgPanel);

    Panel topPanel = new Panel(new BorderLayout());
    topPanel.add(tp, BorderLayout.CENTER);
    topPanel.add(status, BorderLayout.SOUTH);

    tabPanel = new TabbedPanel();
    tabPanel.addItem("Program output", outputPanel);
    tabPanel.addItem("Source code", viewer);

    viewer.setTabPanel(tabPanel);
    outputPanel.setTabPanel(tabPanel);

    setLayout(new BorderLayout());
    add(topPanel, BorderLayout.NORTH);
    add(tabPanel, BorderLayout.CENTER);
  }

  void disableListeners() {
    dbgPanel.disableListeners();
    outputPanel.disableListeners();
  }

  void enableListeners() {
    dbgPanel.enableListeners();
    outputPanel.enableListeners();
  }

  public Dimension getPreferredSize() {
    return new Dimension(600, 600);
  }
}
