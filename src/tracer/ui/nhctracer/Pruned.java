package nhctracer;

import java.awt.*;
import java.util.Vector;

public class Pruned extends EDTNode {
  public Pruned(EDTStructuredNode parent, TraceTree tree, int index) {
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.refnr = 0;
    this.trefnr = 0;
  }

  public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index,
                       int irefnr, NodeTable nt) {
    return new Pruned(parent, tree, index);
  }

  public Object inside(UI ui, int x, int y, int x0, int y0) {
    return (x >= x0 && x <= x0+width ? this : null);
  }

  public int paint(Graphics g, UI ui, int x0, int y0,
                   int refnr, int trefnr, int irefnr) {
    layers = 0;
    g.setColor(color);
    annotate(g, ui, x0, y0, refnr, trefnr, irefnr);
    return width;
  }

  public String dump() {
    return "<P>" + dumprefs();
  }
}
