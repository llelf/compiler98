package nhctracer;

import java.awt.*;
import java.util.Vector;

public class TraceTree extends Object {
  EDTNode node;
  Vector traces;
  Trace parent;
  int index;
  int height;
  int layers;

  public TraceTree(Trace parent) {
    this.parent = parent;
    index = parent.trees.size();
    parent.trees.addElement(this);
    traces = new Vector(3, 3);
  }

  public void addTrace(Trace trace) {
    //trace.color = colors[traces.size()];
    trace.index = traces.size();
    traces.addElement(trace);
  }

  public void childLayers(int layers) {
    if (layers > this.layers)
      this.layers = layers;
  }

  public void setNode(EDTNode node) {
    this.node = node;
  }

  public Trace inTrace(int trefnr) {
    try {
      if (traces != null) {
	for (int i = 0; i < traces.size(); i++) {
	  Trace trace = (Trace)traces.elementAt(i);
	  TraceTree tree = (TraceTree)trace.trees.elementAt(0);
	  if (tree.node.refnr == trefnr)
	    return trace;
	}
      }
      return null;
    } catch (ArrayIndexOutOfBoundsException e) {
      System.err.println("inTrace: bad index\n" + e);
      System.exit(-1);
      return null;
    }
  }

  public Object inside(UI ui, int x, int y, int x0, int y0) {
    int h;
    int cy = y0;

    cy += ui.normalfm.getHeight() + 2*layers + 4;
    if (y <= cy)
      return node.inside(ui, x, y, x0, y0);
    for (int i = 0; i < traces.size(); i++) {
      Trace trace = (Trace)traces.elementAt(i);
      h = trace.height;
      if (!trace.hidden && y <= cy+h)
	return trace.inside(ui, x, y, x0, cy);
      if (!trace.hidden)
	cy += h;
    }
    return null;
  }

  public Dimension paint(Graphics g, UI ui, int x, int y, int refnr, int trefnr, int irefnr) {
    int i;
    int y0 = y, cy = y;
    int maxw;
    Dimension d;
    layers = 0;

    maxw = x + node.paint(g, ui, x, y, refnr, trefnr, irefnr);
    cy += ui.normalfm.getHeight() + 2*layers + 4;
    for (i = 0; i < traces.size(); i++) {
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
