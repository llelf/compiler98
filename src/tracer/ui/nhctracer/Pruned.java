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

  public String getHelpText() {
    return "help text for Pruned";
  }

  public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index, int irefnr, NodeTable nt) {
    return new Pruned(parent, tree, index);
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

    // g.drawString("?", x0-ui.dx, y0-ui.dy+ui.normalfm.getHeight());
    g.drawRect(x0-ui.dx, topline-ui.dy, w, baseline-topline);
    g.drawLine(x0-ui.dx, baseline-ui.dy, x0+w-ui.dx, topline-ui.dy);
    g.drawLine(x0-ui.dx, topline-ui.dy, x0+w-ui.dx, baseline-ui.dy);
    //g.fillRect(x0-ui.dx, topline-ui.dy, w, baseline-topline);

    //underline(g, ui, x0, y0, width);
    annotate(g, ui, x0, y0, refnr, trefnr, irefnr);

    return width;
  }
  public String dump() {
    return "<P>" + dumprefs();
  }
}
