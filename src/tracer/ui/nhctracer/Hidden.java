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

  public String getHelpText() {
    return "help text for Hidden";
  }

  public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index, int irefnr, NodeTable nt) {
    return new Hidden(parent, tree, index, refnr, trefnr);
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
    g.drawLine(x0-ui.dx, baseline-ui.dy, x0+w-ui.dx, topline-ui.dy);
    //      g.drawLine(x0-ui.dx, topline-ui.dy, x0+w-ui.dx, baseline-ui.dy);
    //g.fillRect(x0-ui.dx + w/3, topline-ui.dy,2*w/3, baseline-topline);
    annotate(g, ui, x0, y0, refnr, trefnr, irefnr);

    //underline(g, ui, x0, y0, w);
    return width;
  }

  public String dump() {
    return "<H>" + dumprefs();
  }
}

