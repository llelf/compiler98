package nhctracer;

import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.util.Vector;
import java.io.*;
//import com.sun.java.swing.*;

public class OutputPanel extends Panel {
  TraceFrame frame;
  Output output;
  DbgPanel dbgPanel;
  Canvas canvas;
  Status status;
  TabbedPanel tabPanel;
  Connection serverConnection;
  ScrollPane scrollpane;
  Image offscreen;
  Dimension offscreensize;
  Graphics offgraphics;
  int canvas_width, canvas_height;
  int tree_height = 1, tree_width = 1;
  UI ui;
  public static final int START_X = 0;
  public static final int START_Y = 0;
  int refnr = 0;
  OutputPanel me;
  OutputChar selected;
  int lastline, lastcol;
  MouseHandler mhandler;
  MouseMotionHandler mmhandler;

  public OutputPanel(TraceFrame _frame, Status _status) {
    frame = _frame;
    status = _status;
    me = this;
    setLayout(new BorderLayout());
    setBackground(Color.white);
    canvas = new Canvas() {
      public void paint(Graphics g) {
	me.repaint();
      }
    };
    canvas.setBackground(getBackground());
    canvas.addMouseMotionListener(mmhandler = new MouseMotionHandler());
    canvas.addMouseListener(mhandler = new MouseHandler());
    output = new Output();
    serverConnection = null;
    ui = new UI();
    ui.normalfont = GetParams.getFont("nhctracer.outputfont", Font.PLAIN, "SansSerif", 12);
    ui.boldfont = ui.normalfont;
    ui.normalfm = getFontMetrics(ui.normalfont);	
    ui.boldfm = getFontMetrics(ui.boldfont);	
    setLayout(new BorderLayout(0,0));
    scrollpane = new ScrollPane();
    scrollpane.add(canvas);
    add(scrollpane, BorderLayout.CENTER);
  }

  public void connected(Connection serverConnection) {
    this.serverConnection = serverConnection;
    output = null;
    dbgPanel.serverConnection = serverConnection;
    dbgPanel.ui.resetColors();
    status.setText("Connected to server");
    String t = serverConnection.nextToken();
    if (t.equals("Error")) {
      dbgPanel.connected();
    } else {
      EDTParser parser = new EDTParser(serverConnection, dbgPanel.nodeTable);
      output = parser.parseOutput();
      //tabPanel.setSelectedComponent(this); // Swing
      tabPanel.select("Program output");
      repaint();
    }
  }

  public void disconnected() {
    serverConnection.close();
    output = null;
    dbgPanel.serverConnection = null;
    dbgPanel.trace = null;
    status.setText("Disconnected");
    dbgPanel.repaint();
    repaint();
  }

  public void setDbgPanel(DbgPanel dbgPanel) {
    this.dbgPanel = dbgPanel;
    dbgPanel.serverConnection = serverConnection;
  }

  public void setTabPanel(TabbedPanel tabPanel) {
    this.tabPanel = tabPanel;
  }

  public Dimension getPreferredSize() {
    return new Dimension(400, GetParams.getInt("nhctracer.owpHeight", 200));
  }
  
  public Dimension getMinimumSize() {
    return new Dimension(400, GetParams.getInt("nhctracer.owmHeight", 100));
  }
  
  public void paint(Graphics g) {
    Dimension d = canvas.getSize();
    if ((offscreen == null) || (d.width != offscreensize.width) || 
	(d.height != offscreensize.height)) {
      offscreen = createImage(d.width, d.height);
      if (offscreen == null) {
	System.err.println("Cannot allocate offscreen buffer of size "+
			   d.width + "x" + d.height + ".");
	return;
	/* System.exit(-1); */
      }
      offscreensize = d;
      offgraphics = offscreen.getGraphics();
      offgraphics.setFont(ui.normalfont);
    }
    offgraphics.setColor(getBackground());
    offgraphics.fillRect(0, 0, d.width, d.height);
    if (output != null) {
      Dimension extents = output.paint(offgraphics, ui, START_X, START_Y, refnr);    
      if (Options.oarrow.getState())
	select(lastline, lastcol);
	  
      if (selected != null) {
	Point p = posCoord(output.lineno, output.colno, ui, START_X, START_Y);
	offgraphics.setColor(Color.black);
	int h = 8, w = 4;
	int ax = p.x + START_X, ay = p.y + START_Y;
	int xps[] = {ax+ui.dx, ax+ui.dx+w, ax+ui.dx+w, ax+ui.dx+h}; 
	int yps[] = {ay+ui.dy, ay+ui.dy+h, ay+ui.dy+w, ay+ui.dy+w}; 
	offgraphics.fillPolygon(xps, yps, 4);
	offgraphics.drawPolygon(xps, yps, 4);
	offgraphics.drawLine(ax+ui.dx, ay+ui.dy, ax+ui.dx+h+2, ay+ui.dy+h+2);
      }
      // Reserve some extra space below and to the right      
      extents.width += 24;
      extents.height += 24;
      if (!extents.equals(d))
	canvas.setSize(extents);
    }
    canvas.getGraphics().drawImage(offscreen, 0, 0, null);
  }

  class MouseMotionHandler extends MouseMotionAdapter {
    public void mouseMoved(MouseEvent evt) {
      int x = evt.getX();
      int y = evt.getY();
      if ((evt.getModifiers() & InputEvent.SHIFT_MASK) == 0) {
        if (output == null)
	  return;
        Object obj = output.inside(ui, x+ui.dx, y+ui.dy, START_X, START_Y);
      
	if (obj == null) {	
	  refnr = -1;
	  lastline = lastcol = -1;
	} else if (obj instanceof OutputChar) {
	  if (lastline != output.lineno || lastcol != output.colno) {
	    lastline = output.lineno;
	    lastcol = output.colno;
	    refnr = ((OutputChar)obj).refnr;
	    if (frame != null && frame.script != null)
	      frame.script.println(new ScriptOutputSelect(refnr, output.lineno, output.colno));
	  }
	}
	repaint();
      }
    }
  }

  class MouseHandler extends MouseAdapter {
    public void mouseEntered(MouseEvent evt) {
      canvas.requestFocus();
    }

    public void mouseReleased(MouseEvent evt) {
      repaint();
    }
      
    public void mousePressed(MouseEvent evt) {
      int x = evt.getX();
      int y = evt.getY();
      if (output != null) {
	Object obj;
	if (selected != null)
	  obj = selected;
	else
	  obj = output.inside(ui, x+ui.dx, y+ui.dy, START_X, START_Y);
       
	if (obj instanceof OutputChar) {
	  if (frame != null && frame.script != null)
	    frame.script.println(new ScriptOutputTrail());
	  OutputChar oc = (OutputChar)obj;
	  serverConnection.out.println("Gn 5");
	  serverConnection.out.println(""+oc.refnr);
	  EDTParser parser = new EDTParser(serverConnection, dbgPanel.nodeTable);
	  dbgPanel.trace = parser.parseTrace(null);
	  dbgPanel.trace.color = Color.black;
	  dbgPanel.ui.resetColors();
	  dbgPanel.repaint();
	}
      }
    }
  }

  void disableListeners() {
    canvas.removeMouseMotionListener(mmhandler);
    canvas.removeMouseListener(mhandler);
  }

  void enableListeners() {
    canvas.addMouseMotionListener(mmhandler);
    canvas.addMouseListener(mhandler);
  }

  Point posCoord(int lineno, int colno, UI ui, int startx, int starty) {
    int y = ui.normalfm.getHeight()*lineno+starty+ui.normalfm.getHeight();
    int x = startx;
	
    Vector line = (Vector)output.lines.elementAt(lineno);
    for (int i = 0; i < colno; i++) {
      x += ((OutputChar)line.elementAt(i)).width;
    }
    x += ((OutputChar)line.elementAt(colno)).width/2;
    return new Point(x, y);
  }

  void select(int lineno, int colno) {
    output.lineno = lineno;
    output.colno = colno;
    try {
      selected = (OutputChar)((Vector)output.lines.elementAt(lineno)).elementAt(colno);
    } catch (Exception e) {
      selected = null;
    }
  }

  void unselect() {
      selected = null;
  }
}
