package nhctracer;

import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.util.Vector;
import java.io.*;

public class DbgPanel extends Panel /* implements Runnable */ {
  TraceFrame frame;
  MainPanel mainPanel;
  Thread DbgInterface;
  Canvas canvas;
  SourceViewer viewer = null;
  ScrollPane scrollpane;
  Status status;
  Color mbgc, bgc, fgc, lc;
  Trace trace;
  Object scriptObj;
  EDTNode lastNode = null;
  EDTNode selectedNode = null;
  public static int START_X = 0;
  public static int START_Y = 0;
  Image nullScreen;
  Connection serverConnection;
  UI ui;
  NodeTable nodeTable;
  DbgPanel me;
  FileDialog fd = null;
  int mouseX, mouseY;
  MouseMotionHandler mmhandler;
  MouseHandler mhandler;

  public DbgPanel(TraceFrame _frame, MainPanel _mainPanel) {
    super();
    me = this;
    this.frame = _frame;
    this.mainPanel = _mainPanel;
    
    viewer = mainPanel.viewer;
    status = mainPanel.status;

    setBackground(Color.white);

    canvas = new Canvas() {
      public void paint(Graphics g) {
	me.repaint();
      }
      public Dimension getPreferredSize() {
	return new Dimension(400, 300);
      }
    };
    canvas.setBackground(getBackground());
    canvas.addMouseMotionListener(mmhandler = new MouseMotionHandler());
    canvas.addMouseListener(mhandler = new MouseHandler());

    nodeTable = new NodeTable();

    ui = new UI();
    ui.normalfont =
      GetParams.getFont("nhctracer.tracefont", Font.PLAIN, "SansSerif", 14);
    ui.boldfont = ui.normalfont;
    ui.normalfm = getFontMetrics(ui.normalfont);	
    ui.boldfm = getFontMetrics(ui.boldfont);	

    scrollpane = new ScrollPane();
    scrollpane.add(canvas);

    this.setLayout(new BorderLayout());
    this.add(scrollpane, BorderLayout.CENTER);

    bgc = Color.white;
    fgc = Color.red;
    lc = Color.black;
    mbgc = Color.cyan;

    trace = null;
  }

  void disableListeners() {
    canvas.removeMouseMotionListener(mmhandler);
    canvas.removeMouseListener(mhandler);
  }

  void enableListeners() {
    canvas.addMouseMotionListener(mmhandler);
    canvas.addMouseListener(mhandler);
  }

  public void connected() {
    EDTParser parser = new EDTParser(serverConnection, nodeTable);
    trace = parser.parseTrace(null);
    trace.color = Color.black;
    repaint();
  }

  public void log(String message) {
    if ((frame != null) && (frame.script != null)) {
      frame.script.println(message);
    }
  }
  
  class MouseMotionHandler extends MouseMotionAdapter {
    public void mouseMoved(MouseEvent evt) {
      mouseX = evt.getX();
      mouseY = evt.getY();
      if (trace == null)
	return;
      Object obj = 
	trace.inside(ui, mouseX+ui.dx, mouseY+ui.dy, START_X, START_Y);
      if (obj == null) {	
	if (lastNode != null) {
	  lastNode.color = Color.black;
	  lastNode = null;
	  status.setText("");
	  repaint();
	}
      } else {
	if (obj != lastNode) {
	  if  (lastNode != null) {
	    lastNode.color = Color.black;
	    status.setText("");
	  }
	  if (obj instanceof EDTNode) {
	    ((EDTNode)obj).color = Color.blue;
	    lastNode = (EDTNode)obj;	    
	    if ((frame != null) && (frame.script != null))
	      frame.script.println(new Events.NodeSelect(lastNode.path()));
	  } else if (obj instanceof Trace) {
	    Trace t = (Trace)obj;
	    EDTNode newNode =
	      ((TraceTree)t.trees.elementAt(t.trees.size()-1)).node;
	    while (newNode instanceof Case) {
	      newNode = (EDTNode)((Case)newNode).args.elementAt(1);
	    }
	    if ((frame != null) && (frame.script != null)) {
	      if (newNode != lastNode)
		frame.script.println(new Events.TraceSelect(newNode.path()));
	    }
	    lastNode = newNode;
	  }
	  repaint();
	}
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
      Trace t;
      int x = evt.getX();
      int y = evt.getY();
      int modifiers = evt.getModifiers();
      if (trace == null)
	return;
      
      // determine the selected object
      Object obj;
      if (x < 0) obj = scriptObj;  // running a script
      else obj = trace.inside(ui, x+ui.dx, y+ui.dy, START_X, START_Y);
      if (obj == null) return;
      
      /* Selecting the last whole expression in a trace spine
       * extends the trace itself.  Selecting any other whole
       * expression winds back the trace to make that expression
       * the last.
       */
      if (obj instanceof EDTNode) {
	EDTNode node = (EDTNode)obj;
	if (node.parent == null &&
	    ((modifiers & 
	      (InputEvent.BUTTON2_MASK | InputEvent.BUTTON3_MASK)) == 0)) {
	      int n = node.tree.parent.trees.size();
	      if (node.tree.index == n-1) {
	        obj = node.tree.parent;
              } else {
	        node.tree.parent.trees.setSize(node.tree.index + 1);
		return; 
	      }
        }
      }

      if (obj instanceof Trace) {
	status.setText("Expanding");
	status.waitCursor();
	t = (Trace)obj;
	selectedNode = ((TraceTree)t.trees.elementAt(t.trees.size()-1)).node;
	while (selectedNode instanceof Case) {
	  selectedNode = (EDTNode)((Case)selectedNode).args.elementAt(1);
	}
	if (selectedNode.trefnr == 0) {
	  status.setText("The selected component has no trace");
	} else {
	  if ((frame != null) && (frame.script != null))
	    frame.script.println(new Events.TraceTrail());
	  serverConnection.out.println("Gn 5");
	  serverConnection.out.println(""+selectedNode.trefnr);
	  EDTParser parser = new EDTParser(serverConnection, nodeTable);
	  parser.parseTraceTree(t);
	  lastNode = ((TraceTree)t.trees.elementAt(t.trees.size()-1)).node;
	  status.setText("");
	}
	status.normalCursor();
      } else if (obj instanceof EDTNode) {
	selectedNode = (EDTNode)obj;
	if ((modifiers & InputEvent.BUTTON3_MASK) != 0 &&
	    (modifiers & InputEvent.SHIFT_MASK) != 0) {
 	  /* shift + right click */
	  if (selectedNode instanceof IdName) {
	    IdName name = (IdName)selectedNode;
	    SourceRef sr = new SourceRef(name.module, new Integer(name.defpos));
	    if ((frame != null) && (frame.script != null))
	      frame.script.println(new Events.NodeDefRef());
	    if (sr.col > 0) {
	      status.waitCursor();
	      try {
	        viewer.showSourceLocation(serverConnection,
		  sr.file, sr.line, sr.col);
	      } catch (ArrayIndexOutOfBoundsException e) {
	    	status.setText("No source reference to definition");
	      }
	      status.normalCursor();
            } else {
	      status.setText("No source reference to definition");
	    }
	  }
	} else if ((modifiers & InputEvent.BUTTON3_MASK) != 0) {
	  /* right click */
	  SourceRef sr = selectedNode.sr;
	  if (sr != null && sr.file != null) {
	    status.waitCursor();
	    if ((frame != null) && (frame.script != null))
	      frame.script.println(new Events.NodeSourceRef());
	    try {
	      viewer.showSourceLocation(serverConnection,
	        sr.file, sr.line, sr.col);
	    } catch (ArrayIndexOutOfBoundsException e) {
	      status.setText("No source reference");
            }
	    status.normalCursor();
	  } else {
	    status.setText("No source reference");
	  }
	} else if ((modifiers & InputEvent.BUTTON2_MASK) != 0) {
  	  /* middle click*/
	  if (selectedNode instanceof CutOffTree) {
	    CutOffTree cot = (CutOffTree)selectedNode;
	    status.setText("Expanding");
	    status.waitCursor();
	    if ((frame != null) && (frame.script != null))
	      frame.script.println(new Events.NodeExpand());
	    serverConnection.out.println("R 5");
	    serverConnection.out.println(selectedNode.refnr + " X");
	    EDTParser parser = new EDTParser(serverConnection, nodeTable);
	    EDTNode n = parser.parseEDTNode(cot.parent, cot.tree, cot.index);
	    ((EDTStructuredNode)cot.parent).args.setElementAt(n, cot.index);
	    status.setText("");
	    status.normalCursor();
	  } else {
	    if (obj instanceof EDTStructuredNode) {
	      ((EDTStructuredNode)obj).contract();
	    }
	  }
	} else if ((modifiers & InputEvent.BUTTON1_MASK) != 0) {
	  /* left click */
	  if (selectedNode.trace != null) {
	    selectedNode.trace.hidden = !selectedNode.trace.hidden;
	  } else if ((t = selectedNode.tree.inTrace(selectedNode.trefnr))
	                != null) {
	    selectedNode.setTrace(t);
	    status.setText("");
	  } else {
	    status.setText("Expanding");
	    status.waitCursor();
	    if ((frame != null) && (frame.script != null))
	      frame.script.println(new Events.NodeTrail());
	    if (selectedNode instanceof CutOffTree) {
	      CutOffTree cot = (CutOffTree)selectedNode;
	      serverConnection.out.println("R 5");
	      serverConnection.out.println(selectedNode.refnr + " X");
	      EDTParser parser = new EDTParser(serverConnection, nodeTable);
	      EDTNode n = parser.parseEDTNode(cot.parent, cot.tree, cot.index);
	      cot.parent.args.setElementAt(n, cot.index);
	    } else {
	      if (selectedNode.irefnr == 0) {
	        if (selectedNode.trefnr == 0) {
		  status.setText("The selected component has no trace");
	        } else {
		  serverConnection.out.println("Gn 5");
		  serverConnection.out.println(""+selectedNode.trefnr);
		  EDTParser parser = new EDTParser(serverConnection, nodeTable);
		  t = parser.parseTrace(selectedNode.tree);
		  selectedNode.setTrace(t);
		  selectedNode.tree.addTrace(t);
		  status.setText("");
		} 
              } else {
		serverConnection.out.println("In 5");
		serverConnection.out.println(""+selectedNode.irefnr);
		EDTParser parser = new EDTParser(serverConnection, nodeTable);
		t = parser.parseTrace(selectedNode.tree);
		selectedNode.setTrace(t);
		selectedNode.tree.addTrace(t);
		status.setText("");
	      }
	    }
	  }
	  status.normalCursor();
	}
      }      
    }
  }

public void paint(Graphics g) { 
    
    // First determine the appropriate size for the offscreen buffer.
    // It must be large enough for the trace, if any.
    
    Dimension offsize;
    Dimension cansize = canvas.getSize();
    if (trace == null) offsize = cansize;
    else {
      if (nullScreen == null) nullScreen = createImage(1,1);
      offsize = trace.paint(nullScreen.getGraphics(),ui,0,0,-1,-1,-1);
      offsize.width += scrollpane.getVScrollbarWidth();
      offsize.height += scrollpane.getHScrollbarHeight();
    }
    if (offsize.width < cansize.width) offsize.width = cansize.width;
    if (offsize.height < cansize.height) offsize.height = cansize.height;
    
    // Now paint the trace onto the off-screen buffer.
    
    Image offscreen = createImage(offsize.width,offsize.height);
    Graphics offgraphics = offscreen.getGraphics();
    offgraphics.setFont(ui.normalfont);
    offgraphics.setColor(getBackground());
    offgraphics.fillRect(0, 0, offsize.width, offsize.height);
    int refnr, trefnr, irefnr;
    if (lastNode != null) {
      refnr  = lastNode.refnr;
      trefnr = lastNode.trefnr;
      irefnr = lastNode.irefnr;
    } else 
      refnr = trefnr = irefnr = -1;
    if (trace != null)
      trace.paint(offgraphics, ui, 0, 0, refnr, trefnr, irefnr);
    
    // Finally, transfer the off-screen image to the canvas, which
    // may need to be enlarged.
    
    if (cansize.width < offsize.width || cansize.height < offsize.height)
      canvas.setSize(offsize);
    canvas.getGraphics().drawImage(offscreen,0,0,null);
}
  
  public Dimension getPreferredSize() {
    return new Dimension(400, GetParams.getInt("nhctracer.dwpHeight", 300));
  }
  
  public Dimension getMinimumSize() {
    return new Dimension(400, GetParams.getInt("nhctracer.dwmHeight", 100));
  }
  
}
