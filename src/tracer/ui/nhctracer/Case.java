package nhctracer;

import java.awt.*;
import java.util.Vector;

public class Case extends EDTStructuredNode {
  String key;

  public Case(EDTStructuredNode parent, TraceTree tree, int index) {
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.trace = null;
    args = new Vector(3, 10);
  }

  public Case(EDTStructuredNode parent, TraceTree tree, int index,
              SourceRef sr, int refnr, String key) {
    this.key = key;
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.sr = sr;
    this.refnr = refnr;
    this.trace = null;
    args = new Vector(3, 10);
  }

  public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index,
                       int irefnr, int drefnr, NodeTable nt) {
    Case c = new Case(parent, tree, index);
    c.sr = sr;
    c.key = key;
    c.refnr = refnr;
    c.trefnr = trefnr;
    c.irefnr = irefnr;
    c.drefnr = drefnr;
    for (int i = 0; i < args.size(); i++) {
      EDTNode node = (EDTNode)args.elementAt(i);
      c.args.addElement(node.spawn(c, tree, i, node.irefnr, node.drefnr, nt));
    }
    return c;
  }

  void setArgs(EDTNode se, EDTNode ctxt, int trefnr, int irefnr, int drefnr) {
    args.addElement(se);
    args.addElement(ctxt);
    this.trefnr = trefnr;
    this.irefnr = irefnr;
    this.drefnr = drefnr;
  }

  public Object inside(UI ui, int x, int y, int x0, int y0) {
    int cx = x0;
    EDTNode se = (EDTNode)args.elementAt(0);
    EDTNode ctxt = (EDTNode)args.elementAt(1);
    if (cx < x0) return null;
    if (Options.showcase.getState()) {
      cx += ui.normalfm.stringWidth(key);
      if (x <= cx) return this;
      if (x <= cx+se.width) return se.inside(ui, x, y, cx, y0);      
      cx += se.width + ui.normalfm.stringWidth(" m ");
      if (x <= cx) return null;
    }
    return ctxt.inside(ui, x, y, cx, y0);
  }

  public int paint(Graphics g, UI ui, int x0, int y0,
                   int refnr, int trefnr, int irefnr, int drefnr) {
    layers = 0;
    EDTNode se = (EDTNode)args.elementAt(0);
    EDTNode ctxt = (EDTNode)args.elementAt(1);
    color = Color.black;
    if (drefnr > 0 && drefnr == this.drefnr) {
      color = Color.blue;
    } else if (refnr == this.refnr) {
      if (Options.highshare.getState()) color = Color.green;	      
    }
    g.setColor(color);
    FontMetrics fm = g.getFontMetrics();
    int x = x0;
    if (Options.showcase.getState()) {
      g.drawString(key, x-ui.dx, y0-ui.dy+fm.getHeight());
      x += fm.stringWidth(key);
      x += se.paint(g, ui, x, y0, refnr, trefnr, irefnr, drefnr); 
      g.setColor(Color.black);
      int space = ui.normalfm.charWidth(' ');
      x += space + Symbols.drawWithin(g, ui, x+space, y0) + space;
    }
    x += ctxt.paint(g, ui, x, y0, refnr, trefnr, irefnr, drefnr); 
    width = x-x0;
    g.setFont(ui.normalfont);
    annotate(g, ui, x0, y0, refnr, trefnr, irefnr, drefnr);
    return width;
  }

  public String dump() {
    String res = "(case " + dumprefs();
    for (int i = 0; i < args.size(); i++) {
      res += " " + ((EDTNode)args.elementAt(i)).dump();
    }
    return res + ")";
  }
}

