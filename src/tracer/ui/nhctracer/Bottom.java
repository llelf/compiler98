package nhctracer;

import java.awt.*;
import java.util.Vector;

public class Bottom extends EDTNode {
    
  public Bottom(EDTStructuredNode parent, TraceTree tree, int index, int refnr, int trefnr) {
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.refnr = refnr;
    this.trefnr = trefnr;
  }

  public String getHelpText() {
    return "help text for Bottom";
  }

  public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index, int irefnr, NodeTable nt) {
    return new Bottom(parent, tree, index, refnr, trefnr);
  }

  public Object inside(UI ui, int x, int y, int x0, int y0) {
    return (x >= x0 && x <= x0+width ? this : null);
  }

  public int paint(Graphics g, UI ui, int x0, int y0, int refnr, int trefnr, int irefnr) {
    layers = 0;
    width = ui.normalfm.charWidth('m');
    int w = width * 5 / 6;
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

    annotate(g, ui, x0, y0, refnr, trefnr, irefnr);
    return width;
  }

  public String dump() {
    return "_|_" + dumprefs();
  }
}

