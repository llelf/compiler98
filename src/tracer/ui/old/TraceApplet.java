package nhctracer;

import java.awt.*;
import java.applet.*;
import java.io.*;

public class TraceApplet extends Applet {
  Trace trace;
  EDTNode node;
  Connection conn;
  NodeTable nt;
  EDTNode lastNode = null;
  EDTParser parser;
  UI ui;
  int start_x;
  int start_y;

  public TraceApplet() {
  }

  public void paint(Graphics g) {
    if (conn != null) {
      int refnr, trefnr;
      if (lastNode != null) {
	refnr = lastNode.refnr;
	trefnr = lastNode.trefnr;
      } else {
	refnr = -1;
	trefnr = -1;
      }
      if (trace != null)
	trace.paint(g, ui, start_x, start_y, refnr, trefnr);
      else if (node != null)
	node.paint(g, ui, start_x, start_y, refnr, trefnr);
    }
  }

  public boolean mouseMove(Event evt, int x, int y) {
    Object obj; 

    if (trace != null)
      obj = trace.inside(ui, x+ui.dx, y+ui.dy, start_x, start_y);
    else if (node != null)
      obj = node.inside(ui, x+ui.dx, y+ui.dy, start_x, start_y);
    else 
      return false;

    if (obj == null) {	
      if (lastNode != null) {
	lastNode.color = Color.black;
	lastNode = null;
	repaint();
      }
    } else {
      if (obj != lastNode) {
	if  (lastNode != null)
	  lastNode.color = Color.black;
	if (obj instanceof EDTNode) {
	  ((EDTNode)obj).color = Color.blue;
	  lastNode = (EDTNode)obj;
	} else if (obj instanceof Trace) {
	  lastNode = null;
	}
	repaint();
      }
    }
    return true;
  }
  
  public void init() {
    super.init();
    setBackground(Color.white);
    conn = new Connection(getParameter("expr"));
    String s = getParameter("start_x");
    if (s != null)
      try { start_x = Integer.parseInt(getParameter("start_x")); }
      catch (NumberFormatException e) { start_x = 0; }
    s = getParameter("start_y");
    if (s != null)
      try { start_y = Integer.parseInt(getParameter("start_y")); }
      catch (NumberFormatException e) { start_y = -5; }
    nt = new NodeTable();
    ui = new UI();
    ui.normalfont = new Font("Helvetica", Font.PLAIN, 20/*16*/);
    ui.boldfont = new Font("Helvetica", Font.BOLD, 20/*16*/);
    ui.normalfm = getFontMetrics(ui.normalfont);	
    ui.boldfm = getFontMetrics(ui.boldfont);	
    parser = new EDTParser(conn, nt);
    trace = null;
  }
}

