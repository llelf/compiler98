package nhctracer;

import java.awt.*;

public class Hidden extends EDTNode {
  public Hidden(EDTStructuredNode parent, TraceTree tree, int index, int refnr, int trefnr) {
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.refnr = refnr;
    this.trefnr = trefnr;
  }

  public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index, int irefnr, NodeTable nt) {
    return new Hidden(parent, tree, index, refnr, trefnr);
  }

  public Object inside(UI ui, int x, int y, int x0, int y0) {
    return (x >= x0 && x <= x0+width ? this : null);
  }

  public int paint(Graphics g, UI ui, int x0, int y0,
                   int refnr, int trefnr, int irefnr) {
    layers = 0;
    g.setColor(color);
    width = Symbols.drawHidden(g, ui, x0, y0);
    annotate(g, ui, x0, y0, refnr, trefnr, irefnr);
    return width;
  }

  public String dump() {
    return "<H>" + dumprefs();
  }
}

