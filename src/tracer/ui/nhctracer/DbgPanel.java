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
  TraceCanvas canvas;
  SourceViewer viewer = null;
  TraceScrollPane scrollpane;
  Status status;
  Color mbgc, bgc, lc;
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

    canvas = new TraceCanvas(1200,900);
    nodeTable = new NodeTable();

    ui = new UI();
    ui.normalfont =
      GetParams.getFont("nhctracer.tracefont", Font.BOLD, "SansSerif", 12);
    ui.boldfont = ui.normalfont;
    ui.normalfm = getFontMetrics(ui.normalfont);	
    ui.boldfm = getFontMetrics(ui.boldfont);	

    scrollpane = new TraceScrollPane(ScrollPane.SCROLLBARS_AS_NEEDED, canvas);

    this.setLayout(new BorderLayout());
    this.add(scrollpane, BorderLayout.CENTER);

    bgc = Color.white;
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
    trace = parser.parseTrace(null, -1);
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
	  } else if ((t = selectedNode.tree.inTrace(selectedNode.drefnr))
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
	    } else if (selectedNode.irefnr > 0) {
	      serverConnection.out.println("In 5");
	      serverConnection.out.println(""+selectedNode.irefnr);
	      EDTParser parser = new EDTParser(serverConnection, nodeTable);
	      t = parser.parseTrace(selectedNode.tree, selectedNode.drefnr);
	      selectedNode.setTrace(t);
	      selectedNode.tree.addTrace(t);
	      status.setText("");
	    } else if (selectedNode.trefnr > 0) {
	      serverConnection.out.println("Gn 5");
	      serverConnection.out.println(""+selectedNode.trefnr);
	      EDTParser parser = new EDTParser(serverConnection, nodeTable);
	      t = parser.parseTrace(selectedNode.tree, selectedNode.drefnr);
	      selectedNode.setTrace(t);
	      selectedNode.tree.addTrace(t);
	      status.setText("");
	    } else {
              status.setText("The selected component has no trace");
	    }
	  }
	  status.normalCursor();
	}
      }      
    }
  }

  class TraceCanvas extends Canvas {

    public TraceCanvas(int width, int height) {
      setSize(width,height);
      setBackground(Color.white);
      setBackground(getBackground());
      addMouseMotionListener(mmhandler = new MouseMotionHandler());
      addMouseListener(mhandler = new MouseHandler());
    }

    public void paint(Graphics g) { 

      // First paint the trace onto an off-screen buffer.
      // Image offscreen = createImage(offsize.width,offsize.height);
      Image offscreen = createImage(getSize().width, getSize().height);
      Graphics offgraphics = offscreen.getGraphics();
      offgraphics.setFont(ui.normalfont);
      offgraphics.setColor(getBackground());
      offgraphics.fillRect(0, 0, getSize().width, getSize().height);
      int refnr, trefnr, irefnr, drefnr;
      if (lastNode != null) {
	refnr  = lastNode.refnr;
	trefnr = lastNode.trefnr;
	irefnr = lastNode.irefnr;
	drefnr = lastNode.drefnr;
      } else 
	refnr = trefnr = irefnr = drefnr = -1;
      if (trace != null)
	trace.paint(offgraphics, ui, 0, 0, refnr, trefnr, irefnr, drefnr);

      // Now transfer the off-screen image to the canvas, which
      // may need to be enlarged.
      canvas.getGraphics().drawImage(offscreen,0,0,null);
    }
  }
  
  class TraceScrollPane extends ScrollPane {
    
    public TraceScrollPane(int policy, TraceCanvas canvas) {
      super(policy);
      add(canvas);
    }
    
    public void checksize() {
      if (trace != null) {
	Dimension cansize = canvas.getSize();
	if (nullScreen == null) nullScreen = createImage(1,1);
	Dimension tracesize =
          trace.paint(nullScreen.getGraphics(),ui,0,0,-1,-1,-1,-1);
	if (tracesize.width > cansize.width ||
            tracesize.height > cansize.height) {
	  Point p = getScrollPosition();
	  int newWidth = cansize.width;
	  while (newWidth < tracesize.width) newWidth += 400;
	  int newHeight = cansize.height;
	  while (newHeight < tracesize.height) newHeight += 300;
	  canvas = new TraceCanvas(newWidth, newHeight);
	  addImpl(canvas, null, 0);
	  setScrollPosition(p);
	}
      }
    }
  }
  
  public void paint(Graphics g) {
    scrollpane.checksize();
    canvas.paint(g);
  }
  
  public Dimension getPreferredSize() {
    return new Dimension(400, GetParams.getInt("nhctracer.dwpHeight", 300));
  }
  
  public Dimension getMinimumSize() {
    return new Dimension(400, GetParams.getInt("nhctracer.dwmHeight", 100));
  }
  
}
