package nhctracer;

import java.awt.*;
import java.util.Vector;

public class CutOffTree extends EDTNode {
  public CutOffTree(EDTStructuredNode parent, TraceTree tree, int index,
                    int refnr) {
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.refnr = refnr;
    this.trefnr = -1;
  }

  public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index,
                       int irefnr, int drefnr, NodeTable nt) {
    return new CutOffTree(parent, tree, index, refnr);
  }

  public Object inside(UI ui, int x, int y, int x0, int y0) {
    return (x >= x0 && x <= x0+width ? this : null);
  }

  public int paint(Graphics g, UI ui, int x0, int y0,
                   int refnr, int trefnr, int irefnr, int drefnr) {
    layers = 0;
    g.setColor(color);
    width = Symbols.drawPlaceholder(g, ui, x0, y0);
    annotate(g, ui, x0, y0, refnr, trefnr, irefnr, drefnr);
    return width;
  }

  public String dump() {
    return "<>" + dumprefs();
  }
}

