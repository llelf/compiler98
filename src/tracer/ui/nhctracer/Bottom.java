package nhctracer;

import java.awt.*;
import java.util.Vector;

public class Bottom extends EDTNode {
    
  public Bottom(EDTStructuredNode parent, TraceTree tree, int index,
                int refnr, int trefnr, int drefnr) {
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.refnr = refnr;
    this.trefnr = trefnr;
    this.drefnr = drefnr;
  }

  public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index,
                       int irefnr, int drefnr, NodeTable nt) {
    return new Bottom(parent, tree, index, refnr, trefnr, drefnr);
  }

  public Object inside(UI ui, int x, int y, int x0, int y0) {
    return (x >= x0 && x <= x0+width ? this : null);
  }

  public int paint(Graphics g, UI ui, int x0, int y0,
                   int refnr, int trefnr, int irefnr, int drefnr) {
    layers = 0;
    g.setColor(color);
    if (drefnr > 0 && drefnr == this.drefnr) {
      g.setColor(Color.blue);
    } else if (refnr == this.refnr) {
      if (Options.highshare.getState()) g.setColor(Color.green);
    }
    width = Symbols.drawUndefined(g, ui, x0, y0);	
    annotate(g, ui, x0, y0, refnr, trefnr, irefnr, drefnr);
    return width;
  }

  public String dump() {
    return "_|_" + dumprefs();
  }
}

