package nhctracer;

import java.awt.*;
import java.util.Vector;

public class TraceTree extends Object {
  EDTNode node;		// a root expression
  Vector traces;	// traces for some of its subexpressions
  Trace parent;		// a parent trace to which this tree belongs
  int index;		// position among all trees of the parent
  int height;		// in pixels when drawn on screen
  int layers;		// of underlining beneath the root expression (?)

  public TraceTree(Trace parent) {
    this.parent = parent;
    index = parent.trees.size();
    parent.trees.addElement(this);
    traces = new Vector(3, 3);
  }

  public void addTrace(Trace trace) {
    trace.index = traces.size();
    traces.addElement(trace);
  }
  
  public void delTrace(Trace trace) {
    traces.removeElementAt(trace.index);
    for (int i = trace.index; i < traces.size(); i++) {
      Trace t = (Trace)traces.elementAt(i);
      t.index--;
    }
  }

  public void childLayers(int layers) {
    if (layers > this.layers)
      this.layers = layers;
  }

  public void setNode(EDTNode node) {
    this.node = node;
  }

  public Trace inTrace(int trefnr) {
    // System.err.println("inTrace(iortrefnnr = "+trefnr+")");
    if (traces != null) {
      for (int i = 0; i < traces.size(); i++) {
	Trace trace = (Trace)traces.elementAt(i);
	TraceTree tree = (TraceTree)trace.trees.elementAt(0);
	// System.err.println("  comparing with "+tree.node.refnr);
	if (tree.node.refnr == trefnr) return trace;
      }
    }
    return null;
  }

  public Object inside(UI ui, int x, int y, int x0, int y0) {
    int h;
    int cy = y0;

    cy += ui.normalfm.getHeight() + 2*layers + 6;
    if (y <= cy)
      return node.inside(ui, x, y, x0, y0);
    // component traces are displayed last to first
    // so that each newly requested trace is shown just
    // below the selected expression
    for (int i = traces.size()-1; i >= 0; i--) {
      Trace trace = (Trace)traces.elementAt(i);
      h = trace.height;
      if (!trace.hidden && y <= cy+h)
	return trace.inside(ui, x, y, x0, cy);
      if (!trace.hidden)
	cy += h;
    }
    return null;
  }

  public Dimension paint(Graphics g, UI ui, int x, int y,
                         int refnr, int trefnr, int irefnr) {
    int i;
    int y0 = y, cy = y;
    int maxw;
    Dimension d;
    layers = 0;

    maxw = x + node.paint(g, ui, x, y, refnr, trefnr, irefnr);
    cy += ui.normalfm.getHeight() + 2*layers + 6;
    // component traces are displayed last to first
    // so that each newly requested trace is shown just
    // below the selected expression
    for (i = traces.size()-1; i >= 0; i--) {
      Trace trace = (Trace)traces.elementAt(i);
      if (!trace.hidden) {
	d = trace.paint(g, ui, x, cy, refnr, trefnr, irefnr);
	if (d.width > maxw)
	  maxw = d.width;
	cy += d.height;
      }
    }
    height = cy - y0;
    return new Dimension(maxw, height);
  }
}
