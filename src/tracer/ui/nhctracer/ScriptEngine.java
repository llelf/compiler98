package nhctracer;

import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.io.*;
import java.util.*;

public class ScriptEngine extends Thread {
  Vector scriptv;
  TraceFrame f;
  Frame sframe;
  //Dialog sframe;
  TextArea ta;
  Button step, run, pause, cancel;
  int ix, last;    
  public static final int IDLE = 0;
  public static final int STEPPING = 1;
  public static final int RUNNING = 2;
  public static final int DONE = 3;
  public static final int CANCEL = 4;
  State state;

  ScriptEngine(TraceFrame f, Vector scriptv, String filename) {
    this.f = f;
    this.scriptv = scriptv;
    ix = 0;
    last = scriptv.size()-1;
    sframe = new Frame("Script: " + filename);
    //sframe = new Dialog(f, "Script: " + filename, true);
    sframe.setLayout(new BorderLayout());
    sframe.add(new Label("Comment", Label.CENTER),  BorderLayout.NORTH);
    sframe.add(ta = new TextArea(10, 60), BorderLayout.CENTER);
    ta.setFont(new Font("SansSerif", Font.PLAIN, 18));
    Panel bp = new Panel(new FlowLayout());
    bp.add(step = new Button("Step"));
    bp.add(run = new Button("Run"));
    bp.add(pause = new Button("Pause"));
    bp.add(cancel = new Button("Done"));
    sframe.add(bp, BorderLayout.SOUTH);
    pause.setEnabled(false);
    state = new State();
    step.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
	synchronized (state) {
	  state.set(STEPPING);
	  state.notify();
	}
      }
    });
    run.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
	synchronized (state) {
	  step.setEnabled(false);
	  run.setEnabled(false);
	  state.set(RUNNING);
	  state.notify();
	}
      }
    });
    pause.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
	synchronized (state) {
	  step.setEnabled(true);
	  run.setEnabled(true);
	  pause.setEnabled(false);
	  state.set(IDLE);
	  state.notify();
	}
      }
    });
    cancel.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
	synchronized (state) {
	  state.set(CANCEL);
	  state.notify();
	}
      }
    });
    sframe.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
	synchronized (state) {
	  state.set(CANCEL);
	  state.notify();
	}
      }
    });
    sframe.pack();
    sframe.setVisible(true);
    if (last >= 0 && 
	(Script)scriptv.elementAt(ix) instanceof Events.MetaAutoStart) {
      ix++;
      state.set(RUNNING);
    }
    while (check_nowait()) {
      ix++;
      next();
    }
    start();
  }

  public void run() {
    while (true) {
      synchronized (state) {
	switch (state.get()) {
	case IDLE:
	  break;
	case RUNNING:
	  pause.setEnabled(true);
	case STEPPING:
	  next();
	  while (check_nowait()) {
	    ix++;
	    next();
	  }
	  break;
	case CANCEL:
	  if (f.mainPanel.dbgPanel.lastNode != null)
	    f.mainPanel.dbgPanel.lastNode.selected = false;
	  if ((f.mainPanel.dbgPanel.scriptObj != null) &&
	      (f.mainPanel.dbgPanel.scriptObj instanceof Trace))
	    ((Trace)f.mainPanel.dbgPanel.scriptObj).selected = false; 
	  f.mainPanel.outputPanel.unselect();
	  f.mainPanel.outputPanel.repaint();
	  sframe.setVisible(false);
	  sframe.dispose();
	  f.enableListeners();
	  f.mainPanel.dbgPanel.repaint();
	  return;
	}
	try {
	  if (state.get() == RUNNING)	    
	    sleep(1000);
	  else
	    state.wait();	      
	} catch (InterruptedException e) {
	  System.err.println("ticking...");
	}       
      }
    }
  }
  
  void next() {
    if (ix > last) {
      step.setEnabled(false);
      run.setEnabled(false);
      pause.setEnabled(false);
      ta.setText("Script finished");
      state.set(DONE);
    } else {
      Script script = (Script)scriptv.elementAt(ix++);
      if (script instanceof Events.NodeSelect) {
	Vector path = ((Events.NodeSelect)script).path;
	if (f.mainPanel.dbgPanel.lastNode != null)
	  f.mainPanel.dbgPanel.lastNode.selected = false;
	if ((f.mainPanel.dbgPanel.scriptObj != null) &&
	    (f.mainPanel.dbgPanel.scriptObj instanceof Trace))
	  ((Trace)f.mainPanel.dbgPanel.scriptObj).selected = false; 
	f.mainPanel.dbgPanel.lastNode = f.mainPanel.dbgPanel.trace.find(path);
	f.mainPanel.dbgPanel.lastNode.selected = true;
	f.mainPanel.dbgPanel.scriptObj = f.mainPanel.dbgPanel.lastNode;
      } else if (script instanceof Events.NodeTrail) {
	f.mainPanel.dbgPanel.mhandler.mousePressed(new MouseEvent(f.mainPanel, MouseEvent.MOUSE_PRESSED, 0, InputEvent.BUTTON1_MASK, -1, -1, 1, false));
      } else if (script instanceof Events.NodeExpand) {
	f.mainPanel.dbgPanel.mhandler.mousePressed(new MouseEvent(f.mainPanel, MouseEvent.MOUSE_PRESSED, 0, InputEvent.BUTTON2_MASK, -1, -1, 1, false));
      } else if (script instanceof Events.NodeSourceRef) {
	  f.mainPanel.dbgPanel.mhandler.mousePressed(new MouseEvent(f.mainPanel, MouseEvent.MOUSE_PRESSED, 0, InputEvent.BUTTON3_MASK, -1, -1, 1, false));
      } else if (script instanceof Events.NodeDefRef) {
	  f.mainPanel.dbgPanel.mhandler.mousePressed(new MouseEvent(f.mainPanel, MouseEvent.MOUSE_PRESSED, 0, InputEvent.BUTTON3_MASK | InputEvent.BUTTON3_MASK, -1, -1, 1, false));
	  
      } else if (script instanceof Events.TraceSelect) {
	Vector path = ((Events.TraceSelect)script).path;
	if (f.mainPanel.dbgPanel.lastNode != null)
	  f.mainPanel.dbgPanel.lastNode.selected = false;
	if ((f.mainPanel.dbgPanel.scriptObj != null) &&
	    (f.mainPanel.dbgPanel.scriptObj instanceof Trace))
	  ((Trace)f.mainPanel.dbgPanel.scriptObj).selected = false; 
	f.mainPanel.dbgPanel.lastNode = f.mainPanel.dbgPanel.trace.find(path);
	f.mainPanel.dbgPanel.scriptObj = 
	    f.mainPanel.dbgPanel.lastNode.tree.parent;
	((Trace)f.mainPanel.dbgPanel.scriptObj).selected = true; 
      } else if (script instanceof Events.TraceTrail) {
	f.mainPanel.dbgPanel.mhandler.mousePressed(new MouseEvent(f.mainPanel, MouseEvent.MOUSE_PRESSED, 0, InputEvent.BUTTON1_MASK, -1, -1, 1, false));
      } else if (script instanceof Events.OutputSelect) {
	Events.OutputSelect sos = (Events.OutputSelect)script;
	f.mainPanel.outputPanel.refnr = sos.refnr;
	f.mainPanel.outputPanel.select(sos.lineno, sos.colno);
	f.mainPanel.outputPanel.repaint();
      } else if (script instanceof Events.OutputTrail) {
	f.mainPanel.outputPanel.mhandler.mousePressed(new MouseEvent(f.mainPanel, MouseEvent.MOUSE_PRESSED, 0, InputEvent.BUTTON1_MASK, -1, -1, 1, false));
      } else if (script instanceof Events.MetaConnect) {
	f.host = ((Events.MetaConnect)script).host;
	f.port = ((Events.MetaConnect)script).port;
	ta.setText("Connecting to " + f.host + " at port " + f.port);
	f.handler.actionPerformed(new ActionEvent(f.connect, 0, ""));
	if (f.serverConnection == null) {
	  step.setEnabled(false);
	  run.setEnabled(false);
	  pause.setEnabled(false);
	  f.mainPanel.status.setStateText("");
	  ta.setText("Script aborted because of a server connection error.\n" +
	      "Reason: " + f.mainPanel.dbgPanel.status.getText());
	  state.set(DONE);
	}
      } else if (script instanceof Events.MetaDisconnect) {
	ta.setText("Disconnecting.");
	f.handler.actionPerformed(new ActionEvent(f.disconnect, 0, ""));
      } else if (script instanceof Events.MetaMessage) {
	ta.setText(((Events.MetaMessage)script).message);
      } else if (script instanceof Events.MetaAutoQuit) {
	state.set(CANCEL);
      } else {
	System.err.println("Unhandled script item: " + script);
      }
      f.mainPanel.dbgPanel.repaint();
      if ( !(script instanceof Events.MetaMessage)
	&& ix <= last
	&& (script = (Script)scriptv.elementAt(ix)) instanceof 
	    Events.MetaMessage) {
	next();
      }
      //Toolkit.getDefaultToolkit().sync();
    }
  }

  boolean check_nowait() {
    Script script;
    return (ix < last &&
	    state.get() != DONE &&
	    (script = (Script)scriptv.elementAt(ix)) instanceof 
	    Events.MetaNoWait);
  }

  class State {
    int stateval;
    
    State() {
      stateval = IDLE;
    }

    int get() {
      return stateval;
    }

    void set(int val) {
      stateval = val;
    }
  }
}
