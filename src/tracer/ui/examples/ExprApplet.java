package nhctracer;
import java.awt.*;
import java.applet.*;
import java.io.*;
import Tree;
import EDTParser;
import Connection;

public class ExprApplet extends Applet {
  Trace trace;
  Connection conn;
  NodeTable nt;
  EDTNode lastNode = null;
  UI ui;
  private static final int START_X = 0;
  private static final int START_Y = -5;

  public ExprApplet() {
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
      trace.paint(g, ui, START_X, START_Y, refnr, trefnr);
    }
  }

  public boolean mouseMove(Event evt, int x, int y) {
    if (trace == null)
      return false;
    Object obj = trace.inside(ui, x+ui.dx, y+ui.dy, START_X, START_Y);
    
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
    nt = new NodeTable();
    ui = new UI();
    ui.normalfont = new Font("Helvetica", Font.PLAIN, 20/*16*/);
    ui.boldfont = new Font("Helvetica", Font.BOLD, 20/*16*/);
    ui.normalfm = getFontMetrics(ui.normalfont);	
    ui.boldfm = getFontMetrics(ui.boldfont);	
    EDTParser parser = new EDTParser(conn, nt);
    trace = parser.parseTrace(null);
    trace.color = Color.black;
  }
}

