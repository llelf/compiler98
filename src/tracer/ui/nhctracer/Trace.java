package nhctracer;

import java.awt.*;
import java.util.Vector;

public class Trace extends Object {
    TraceTree parent;
    Vector trees;
    Color color;
    int x0, y0, height;
    boolean hidden;
    int index;
    boolean selected = false;
    public static final int INDENT = 20;
    public static final int RADIUS = (INDENT-4)/2;

    public Trace(TraceTree parent/*, int index*/) {
      this.parent = parent;
      trees = new Vector(3, 3);
      hidden = false;
    }

    public Object inside(UI ui, int x, int y, int x0, int y0) {
      int h;
      int cy = y0;
      Object result;
      Trace trace;
	
      // only the arrowhead of the trace bar itself is selectable
      if (x >= x0 && x <= x0+2*RADIUS &&
          y >= y0+height-ui.normalfm.getHeight()/3)
	return this;
      // other selections in component trees of the trace
      for (int i = 0; i < trees.size(); i++) {
	TraceTree tree = (TraceTree)trees.elementAt(i);	    
	if (y < cy+tree.height)
	  return tree.inside(ui, x, y, x0+INDENT, cy);
	cy += tree.height;
      }
      return null;
    }

    public Dimension paint(Graphics g, UI ui, int x, int y, int refnr, int trefnr, int irefnr) {
      int i;
      int cy = y;
      int maxw = 0;
      TraceTree tree = null;
      Dimension d;

      x0 = x;
      y0 = y;
      for (i = 0; i < trees.size(); i++) {
	tree = (TraceTree)trees.elementAt(i);
	d = tree.paint(g, ui, x + INDENT, cy, refnr, trefnr, irefnr);
	if (d.width > maxw)
	  maxw = d.width;
	cy += d.height;
      }
      height = cy - y0;
      if (color == null)
	color = ui.getColor();
      g.setColor(color);
      g.fillArc(x-ui.dx+RADIUS/2, y0-ui.dy+ui.normalfm.getHeight()/2-3,
		RADIUS, RADIUS, 0, 360);
      g.drawLine(x-ui.dx+RADIUS, cy-ui.dy, x-ui.dx+RADIUS, 
		 y0-ui.dy+ui.normalfm.getHeight()/2-3);
      if (tree != null && tree.node.trefnr == 0) {
	g.drawLine(x-ui.dx, cy-ui.dy-1, x-ui.dx+2*RADIUS, cy-ui.dy-1);
	g.drawLine(x-ui.dx, cy-ui.dy, x-ui.dx+2*RADIUS, cy-ui.dy);
      } else {
	int xPoints[] = {x-ui.dx, x-ui.dx+2*RADIUS, x-ui.dx+RADIUS};
	int yPoints[] = {cy-ui.dy-ui.normalfm.getHeight()/3+3,
			 cy-ui.dy-ui.normalfm.getHeight()/3+3,
			 cy-ui.dy+3};
	g.fillPolygon(xPoints, yPoints, 3);
      }
      if (selected) {
	g.setColor(Color.black);
	int h = 8, w = 4;
	int ax = x + RADIUS, ay = cy;
	int xps[] = {ax+ui.dx, ax+ui.dx+w, ax+ui.dx+w, ax+ui.dx+h}; 
	int yps[] = {ay+ui.dy, ay+ui.dy+h, ay+ui.dy+w, ay+ui.dy+w}; 
	g.fillPolygon(xps, yps, 4);
	g.drawPolygon(xps, yps, 4);
	g.drawLine(ax+ui.dx, ay+ui.dy, ax+ui.dx+h+2, ay+ui.dy+h+2);
      }
      return new Dimension(maxw, height);
    }

  public EDTNode find(Vector v) {
    int vix, tix, nix;
    vix = v.size()-2;
    tix = ((Integer)v.elementAt(vix--)).intValue();
    TraceTree tree = (TraceTree)trees.elementAt(tix-1);

    while (vix >= 0 && (tix = ((Integer)v.elementAt(vix)).intValue()) > 0) { 
      vix--;
      Trace trace = (Trace)tree.traces.elementAt(tix-1);
      tix = ((Integer)v.elementAt(vix--)).intValue();
      tree = (TraceTree)trace.trees.elementAt(tix-1); 
    }
    EDTNode node = tree.node;
    while (vix >= 0) {
      nix = -((Integer)v.elementAt(vix--)).intValue()-1;
      node = (EDTNode)((EDTStructuredNode)node).args.elementAt(nix);
    }
    return node;
  }
}
