package nhctracer;

import java.awt.*;
import java.util.Vector;
import Connection;



class Bottom extends EDTNode {
    public Bottom(EDTStructuredNode parent, TraceTree tree, int index, int refnr, int trefnr) {
	this.parent = parent;
	this.tree = tree;
	this.index = index;
	this.refnr = refnr;
	this.trefnr = trefnr;
    }

    public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index) {
        return new Bottom(parent, tree, index, refnr, trefnr);
    }

    public Object inside(UI ui, int x, int y, int x0, int y0) {
	return (x >= x0 && x <= x0+width ? this : null);
    }

    public int paint(Graphics g, UI ui, int x0_, int y0, int refnr, int trefnr) {
	width = ui.normalfm.charWidth('m');
	int w = width * 5 / 6;
	x0 = x0_;
	g.setColor(color);
	if (this.trefnr >= 0)
	  if (refnr == this.refnr) {
	      g.setColor(Color.blue);
	  } else if (trefnr == this.trefnr)
	      g.setColor(Color.red);
	
	int baseline = y0 + ui.normalfm.getHeight();
	int topline = baseline - ui.normalfm.getAscent()*5/6;

	g.drawLine(x0-ui.dx, baseline-ui.dy, x0+w-ui.dx, baseline-ui.dy);
	g.drawLine(x0-ui.dx, baseline-1-ui.dy, x0+w-ui.dx, baseline-1-ui.dy);
	g.drawLine(x0+w/2-ui.dx, baseline-ui.dy, x0+w/2-ui.dx, topline-ui.dy);
	g.drawLine(x0+w/2+1-ui.dx, baseline-ui.dy, x0+w/2+1-ui.dx, topline-ui.dy);

	underline(g, ui, x0, y0, w);

	return width;
    }
}

class Pruned extends EDTNode {
    public Pruned(EDTStructuredNode parent, TraceTree tree, int index) {
	this.parent = parent;
	this.tree = tree;
	this.index = index;
	this.refnr = 0;
	this.trefnr = 0;
    }

    public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index) {
        return new Pruned(parent, tree, index);
    }

    public Object inside(UI ui, int x, int y, int x0, int y0) {
	return (x >= x0 && x <= x0+width ? this : null);
    }

    public int paint(Graphics g, UI ui, int x0_, int y0, int refnr, int trefnr) {
	width = ui.normalfm.charWidth('?');
	x0 = x0_;
	g.setColor(color);
	
	int baseline = y0 + ui.normalfm.getHeight();
	int topline = baseline - ui.normalfm.getAscent()*5/6;

	g.drawString("?", x0-ui.dx, y0-ui.dy+ui.normalfm.getHeight());

	underline(g, ui, x0, y0, width);

	return width;
    }
}

// class CallTree extends Object {
//     Trace parent;
//     EDTNode node;
//     Vector traces;
//     int x0, y0, height;
//     int index;
//     public static final Color colors[] = 
// 	{Color.blue, Color.red, Color.green, Color.yellow, Color.cyan,
// 	 Color.magenta, Color.black, Color.gray};
//     public static final int INDENT = 32;

//     public String path() {
// 	if (parent == null)
// 	    return "";
// 	else
// 	    return parent.path() + " D" + index;
//     }
// }



