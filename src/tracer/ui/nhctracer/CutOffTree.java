package nhctracer;

import java.awt.*;
import java.util.Vector;

public class CutOffTree extends EDTNode {
  public CutOffTree(EDTStructuredNode parent, TraceTree tree, int index, int refnr) {
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.refnr = refnr;
    this.trefnr = -1;
  }

  public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index, int irefnr, NodeTable nt) {
    return new CutOffTree(parent, tree, index, refnr);
  }

  public Object inside(UI ui, int x, int y, int x0, int y0) {
    return (x >= x0 && x <= x0+width ? this : null);
  }

  public int paint(Graphics g, UI ui, int x0, int y0, int refnr, int trefnr, int irefnr) {
    layers = 0;
    width = ui.normalfm.charWidth('m');
    int w = width * 5 / 6;
    g.setColor(color);
	
    int baseline = y0 + ui.normalfm.getHeight();
    int topline = baseline - ui.normalfm.getAscent()*5/6;

    g.drawRect(x0-ui.dx, topline-ui.dy, w, baseline-topline);
    //g.fillRect(x0-ui.dx + 2*w/3, topline-ui.dy, w/3, baseline-topline);

    annotate(g, ui, x0, y0, refnr, trefnr, irefnr);

    return width;
  }

  public String dump() {
    return "<>" + dumprefs();
  }
}

