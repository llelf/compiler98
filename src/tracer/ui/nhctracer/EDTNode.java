package nhctracer;

import java.awt.*;
import java.util.Vector;

/**
<i>EDTNode</i> is the basic node class in the redex trail tree. All other
node classes extend this class. It has a number of abstract methods that 
concrete sub-classes must implement.
 */
public abstract class EDTNode extends Object {
  /** The parent node of this object, or <i>null</i> if none */
  EDTStructuredNode parent;
  /** The tree this node belongs to */
  TraceTree tree;
  /** The trace this node belongs to */
  Trace trace;
  /** Source reference for this node */
  SourceRef sr;
  /** The colour we should be painted with */
  Color color = Color.black;
  /** Our reference number */
  int refnr;
  /** If > 0, this is an indirection to another node with that ref. number */
  int irefnr = 0;
  /** The reference number of our parent */
  int trefnr;
  /** The index in our parent's children list */
  int index;
  /** Our (cached) graphical width */
  int width;
  /* int x0; */
  /** The level of underlinings for this node */
  int layers;
  /** True if the node is an expression that should be shown infix */
  boolean infix = false;
  /** True if the node is the selected one (used when running scripts) */
  boolean selected = false;

  /**
   * Checks if the mouse pointer is inside the graphical representation 
   * of this object.
   * 
   * @param     ui the current user interface state
   * @param     x the current mouse x-coordinate 
   * @param     y the current mouse y-coordinate 
   * @param     x0 the x-coordinate of "our" start position
   * @param     y0 the y-coordinate of "our" start position
   * @return    "this" if inside, <i>null</i> otherwise
   */
  abstract public Object inside(UI ui, int x, int y, int x0, int y0);

  /**
   * Paints the object
   * 
   * @param     g the current graphical context
   * @param     ui the current user interface state
   * @param     x0_ the x-coordinate the position where we should paint
   * @param     y0_ the y-coordinate the position where we should paint
   * @param     refnr the current node reference number.
   * @param     trefnr the current parent node reference number.
   * @param     irefnr the current indirection node reference number.
   * @param     nt the current name table (doesn't seem to be used anymore)
   * @return    the width of our graphical representation
   */
  abstract public int paint(Graphics g, UI ui, int x0_, int y0, int refnr, int trefnr, int irefnr);

  /**
   * Clones the object
   * 
   * @param     parent the new parent
   * @param     tree the new tree
   * @param     index the new index
   * @param     irefnr the new index reference number
   * @param     nt the current name table
   * @return    the new copy of the cloned object
   */
  abstract public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index, int irefnr, NodeTable nt);

  /**
   * Returns the status line help text for node classes
   */
  abstract public String getHelpText();

  /**
   * Set the trace of the node
   * 
   * @param     trace the new trace
   */
  public void setTrace(Trace trace) {
    this.trace = trace;
  }

  /**
   * Set the reference numbers of the node
   * 
   * @param     trefnr the parent trail reference number
   * @param     irefnr the indirection node reference number
   */
  public void setTRefNr(int trefnr, int irefnr) {
    this.trefnr = trefnr;
    this.irefnr = irefnr;
  }

    
  /**
   * Annotate the node depending on the context. The same arguments as to 
   * <i>paint</i>.
   */
  public void annotate(Graphics g, UI ui, int x0, int y0, int refnr, int trefnr, int irefnr) {
    if (refnr == this.refnr)
      boxify(g, ui, x0, y0, width);
    underline(g, ui, x0, y0, width);
    check_ancestor(g, ui, x0, y0, refnr, trefnr, irefnr);
    check_selected(g, ui, x0, y0, refnr, trefnr, irefnr);
  }

  /**
   * Draw a box around the graphical representation of the object.
   */
  void boxify(Graphics g, UI ui, int x0, int y0, int width) {
    g.setColor(Color.red);
    int baseline = y0 + 5 + ui.normalfm.getHeight();
    int topline = y0+3; //baseline - ui.normalfm.getAscent();
    g.drawRect(x0-3-ui.dx, topline+1-ui.dy, 
	       width+6, baseline-topline-2);	    
    g.drawRect(x0-2-ui.dx, topline-ui.dy, 
	       width+4, baseline-topline);	          
    /*
      g.drawRect(x0-ui.normalfm.charWidth(' ')/2-ui.dx, topline+1-ui.dy, 
      width+2, baseline-topline-2);	    
      g.drawRect(x0-ui.normalfm.charWidth(' ')/2+1-ui.dx, topline-ui.dy, 
      width, baseline-topline);	          
    */
  }

  /**
   * Underline the graphical representation of the object.
   */
  void underline(Graphics g, UI ui, int x, int y, int width) {
    int i;
    if (trace != null) {
      layers++;
      int ly = y + ui.normalfm.getHeight() + 2 + 2*layers;
      if (trace.hidden) {
	g.setColor(Color.black);
	for (i = x; i < x+width-3; i += 6) {
	  g.drawLine(i-ui.dx, ly-ui.dy, i+3-ui.dx, ly-ui.dy);
	  g.drawLine(i-ui.dx, ly+1-ui.dy, i+3-ui.dx, ly+1-ui.dy);
	}
      } else {
	g.setColor(trace.color);
	g.drawLine(x-ui.dx, ly-ui.dy, x+width-ui.dx, ly-ui.dy);
	g.drawLine(x-ui.dx, ly+1-ui.dy, x+width-ui.dx, ly+1-ui.dy);
      }
    }	
    if (parent == null)
      tree.childLayers(layers);
    else
      parent.childLayers(layers);
  }

  /**
   * Highlight background if we are the immediate ancestor to the current node.
   */
  void check_ancestor(Graphics g, UI ui, int x0, int y0, int refnr, int trefnr, int irefnr) {
    if (this.refnr == trefnr && !ui.highlighting()) {
      ui.setHighlighting();
      // Paint background yellow, then paint our self again
      g.setColor(Color.yellow);
      g.fillRect(x0-ui.dx, y0-ui.dy+ui.normalfm.getDescent(), 
		 width, ui.normalfm.getHeight());
      this.paint(g, ui, x0, y0, refnr, trefnr, irefnr);
      ui.clearHighlighting();
    }
  }
  
  /**
   * Draw something looking like a mouse cursor over the object. 
   * Used when printing sceen dumps.
   */
  public void check_selected(Graphics g, UI ui, int x0, int y0, int refnr, int trefnr, int irefnr) {
    if (selected) {
      g.setColor(Color.black);
      int h = 8, w = 4;
      int x = x0 + width/2, y = y0 + g.getFontMetrics().getHeight();
      int xps[] = {x+ui.dx, x+ui.dx+w, x+ui.dx+w, x+ui.dx+h}; 
      int yps[] = {y+ui.dy, y+ui.dy+h, y+ui.dy+w, y+ui.dy+w}; 
      g.fillPolygon(xps, yps, 4);
      g.drawPolygon(xps, yps, 4);
      g.drawLine(x+ui.dx, y+ui.dy, x+ui.dx+h+2, y+ui.dy+h+2);
    }
  }

  /**
   * Dumps a string representation of the node. 
   * Used when debugging the implementation.
   */
  abstract public String dump();

  /**
   * Dumps reference numbers when dumping the object.
   * Used when debugging the implementation.
   */
  public String dumprefs() {
    return Options.dumprefs.getState() ? "{" + refnr + "}" : "";
  }

  /**
   * Another way of textually dump the object.
   * Used when debugging the implementation.
   */
  public String show() {
    return 
      "" + this + "\n" +
      "refnr  = " + refnr + "\n" +
      "trefnr = " + trefnr + "\n";
  }

  /**
   * Returns the "path" in the node tree for the current node.
   */
  public Vector path() {
    Vector v = new Vector();
    EDTNode node = this;
    while (node.index != -1) {
      v.addElement(new Integer(-(node.index+1)));
      node = node.parent;
    }
    TraceTree ntree = node.tree;
    Trace ntrace;
    while (ntree != null) {
      v.addElement(new Integer(ntree.index+1));
      ntrace = ntree.parent;
      v.addElement(new Integer(ntrace.index > 0 ? ntrace.index+1 : 1));      
      ntree = ntrace.parent;
    }
    return v;
  } 
  
  /**
   * Returns the a textual version of the node "path".
   */
  public String pathStr() {
    Vector v = path();
    String s = "{";
    String mid = null;
    for (int i = v.size()-1; i >=0; i--) {
      if (mid == null)
	mid = ", ";
      else
	s += mid;
      s += v.elementAt(i);
    }
    return s + "}";
  }
}
