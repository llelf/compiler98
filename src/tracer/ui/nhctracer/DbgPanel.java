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
  // Image offscreen;
  // Dimension offscreensize;
  // Graphics offgraphics;
  Image nullScreen;
  Connection serverConnection;
  UI ui;
  NodeTable nodeTable;
  DbgPanel me;
  boolean doPrint = false;
  FileDialog fd = null;
  int mouseX, mouseY;
  MouseMotionHandler mmhandler;
  MouseHandler mhandler;
  // KeyHandler khandler;

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
    // canvas.addKeyListener(khandler = new KeyHandler());

    nodeTable = new NodeTable();

    ui = new UI();
    ui.normalfont = GetParams.getFont("nhctracer.tracefont", Font.PLAIN, "SansSerif", 18);
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
    // canvas.removeKeyListener(khandler);
  }

  void enableListeners() {
    canvas.addMouseMotionListener(mmhandler);
    canvas.addMouseListener(mhandler);
    // canvas.addKeyListener(khandler);
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

  // a secret key that may have worked once, when
  // an old paint method printed to file instead
  // if doPrint was set
  /*
  class KeyHandler extends KeyAdapter {
    public void keyPressed(KeyEvent evt) {
      switch (evt.getKeyChar()) {
      case 'p':
	if (evt.isMetaDown()) {
	  me.doPrint = true;
	  me.repaint();
	}
      }
    }
  }
  */
  
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
	    EDTNode newNode = ((TraceTree)t.trees.elementAt(t.trees.size()-1)).node;
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
      
      // remember current scrolling position
      // Point scrollPt = scrollpane.getScrollPosition();
      
      // Selecting the last whole expression in a trace spine
      // extends the trace itself.  Selecting any other whole
      // expression winds back the trace to make that expression
      // the last.
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
		// repaint();
		// scrollpane.doLayout();
		// scrollpane.setScrollPosition(scrollPt);
		// scrollpane.invalidate();
		// scrollpane.validate();return;
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
	    (modifiers & InputEvent.SHIFT_MASK) != 0) {	// Shift-right click
	  if (selectedNode instanceof IdName) {
	    IdName name = (IdName)selectedNode;
	    SourceRef sr = new SourceRef(name.module, new Integer(name.defpos));
	    if ((frame != null) && (frame.script != null))
	      frame.script.println(new Events.NodeDefRef());
	    if (sr.col > 0) {
	      status.waitCursor();
	      try {
	        viewer.showSourceLocation(serverConnection, sr.file, sr.line, sr.col);
	      } catch (ArrayIndexOutOfBoundsException e) {
	    	status.setText("No source reference to definition");
	      }
	      status.normalCursor();
            } else {
	      status.setText("No source reference to definition");
	    }
	  }
	} else if ((modifiers & InputEvent.BUTTON3_MASK) != 0) {	// Right click
	  SourceRef sr = selectedNode.sr;
	  if (sr != null && sr.file != null) {
	    status.waitCursor();
	    if ((frame != null) && (frame.script != null))
	      frame.script.println(new Events.NodeSourceRef());
	    try {
	      viewer.showSourceLocation(serverConnection, sr.file, sr.line, sr.col);
	    } catch (ArrayIndexOutOfBoundsException e) {
	      status.setText("No source reference");
            }
	    status.normalCursor();
	  } else {
	    status.setText("No source reference");
	  }
	} else if ((modifiers & InputEvent.BUTTON2_MASK) != 0 &&
		   (modifiers & InputEvent.SHIFT_MASK) != 0) { // Shift-middle click
	  if (selectedNode instanceof HString)
	    ((HString)selectedNode).consify();
	  else
	    if (!HString.tryStringify(selectedNode))
	      status.setText("Selected component is not a string");
	  // repaint();
	} else if ((modifiers & InputEvent.BUTTON2_MASK) != 0) {	// Middle click
	  if (selectedNode instanceof CutOffTree) { // Expand it
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
	} else if ((modifiers & InputEvent.SHIFT_MASK) != 0) { // Shift-left click
	  System.err.println(selectedNode.show());
	} else {						// Left click
	  if (selectedNode.trace != null) {
	    selectedNode.trace.hidden = !selectedNode.trace.hidden;
	  } else if ((t = selectedNode.tree.inTrace(selectedNode.trefnr)) != null) {
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
	      if (selectedNode.trefnr == 0) {
		status.setText("The selected component has no trace");
	      } else {
		if (selectedNode.irefnr == 0) {
		  serverConnection.out.println("Gn 5");
		  serverConnection.out.println(""+selectedNode.trefnr);
		  EDTParser parser = new EDTParser(serverConnection, nodeTable);
		  t = parser.parseTrace(selectedNode.tree);
		  selectedNode.setTrace(t);
		  selectedNode.tree.addTrace(t);
		  status.setText("");
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
	  }
	  status.normalCursor();
	  // repaint();
	}
      }      
      // scrollpane.doLayout();
      // scrollpane.setScrollPosition(scrollPt);
      // scrollpane.invalidate();
      // scrollpane.validate();
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
      offsize.width += 24;
      offsize.height += 24;
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

/*
public void paint{Graphics g) {
    Dimension d = canvas.getSize();
    int start_x = START_X, start_y = START_Y;
    
    if ((offscreen == null) || (d.width != offscreensize.width) || 
	(d.height != offscreensize.height)) {
      offscreen = createImage(d.width, d.height);
      if (offscreen == null) {
	System.err.println("Cannot allocate offscreen buffer of size "+
			   d.width + "x" + d.height + ".");
	return;
	// System.exit(-1);
      }
      offscreensize = d;
    }
    offgraphics = offscreen.getGraphics();
    offgraphics.setFont(ui.normalfont);

    if (me.doPrint) {
      PrintJob pj = Toolkit.getDefaultToolkit().getPrintJob(frame, "NhcTracer image", null);
      if (pj != null) {
	offgraphics = pj.getGraphics();
	//pjg.drawImage(offscreen, 10, 10, null);
	//pjg.dispose();      	
      }      
      start_x += 50;
      start_y += 50;
    } else {
      offgraphics.setColor(getBackground());
      offgraphics.fillRect(0, 0, d.width, d.height);
    }

    int refnr, trefnr, irefnr;
    if (lastNode != null) {
      refnr = lastNode.refnr;
      trefnr = lastNode.trefnr;
      irefnr = lastNode.irefnr;
    } else {
      refnr = -1;
      trefnr = -1;
      irefnr = -1;
    }
    if (trace != null) {
      Dimension extents = trace.paint(offgraphics, ui, start_x, start_y, refnr, trefnr, irefnr);
      // Reserve some extra space below and to the right
      extents.width += 24;
      extents.height += 24;
      // if (!extents.equals(d) && !me.doPrint)
      //   canvas.setSize(extents);
    }
    if (me.doPrint) {
      if (Options.arrow.getState()) { // Paint a cursor?
	int h = 8, w = 4;
	mouseX += 50;
	mouseY += 50;
	int xps[] = 
	  {mouseX+ui.dx, mouseX+ui.dx+w, mouseX+ui.dx+w, mouseX+ui.dx+h}; 
	int yps[] = 
	  {mouseY+ui.dy, mouseY+ui.dy+h, mouseY+ui.dy+w, mouseY+ui.dy+w}; 
	offgraphics.fillPolygon(xps, yps, 4);
	offgraphics.drawPolygon(xps, yps, 4);
	offgraphics.drawLine(mouseX+ui.dx, mouseY+ui.dy, mouseX+ui.dx+h+2, mouseY+ui.dy+h+2);
	//offgraphics.drawLine(mouseX+ui.dx+1, mouseY+ui.dy, mouseX+ui.dx+13, mouseY+12+ui.dy);
      }
      // begin comment
      try {
	if (fd == null)
	  fd = new FileDialog(frame, "Create GIF image", FileDialog.SAVE);
	fd.show();
	if (fd.getFile() != null) {
	  GIFEncoder ge = new GIFEncoder(offscreen);
	  OutputStream output = 
	    new BufferedOutputStream(new FileOutputStream(fd.getFile()));
	  ge.Write(output);
	}
      } catch (IOException e) {
	System.err.println("Couldn't write GIF file: " + e);
      } catch (AWTException e) {
	System.err.println("Couldn't create GIF encoder: " + e);
      }
      // end comment
      offgraphics.dispose();
      me.doPrint = false;
    } else {
      canvas.getGraphics().drawImage(offscreen, 0, 0, null);
    }
  }
*/
  
  public Dimension getPreferredSize() {
    return new Dimension(400, GetParams.getInt("nhctracer.dwpHeight", 300));
  }
  
  public Dimension getMinimumSize() {
    return new Dimension(400, GetParams.getInt("nhctracer.dwmHeight", 100));
  }
  
}
