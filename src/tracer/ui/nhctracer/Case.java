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

  public Case(EDTStructuredNode parent, TraceTree tree, int index, SourceRef sr, int refnr, String key) {
    this.key = key;
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.sr = sr;
    this.refnr = refnr;
    this.trace = null;
    args = new Vector(3, 10);
  }

  public String getHelpText() {
    return "help text for Case";
  }

  public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index, int irefnr, NodeTable nt) {
    Case c = new Case(parent, tree, index);
    c.sr = sr;
    c.key = key;
    c.refnr = refnr;
    c.trefnr = trefnr;
    c.irefnr = irefnr;

    for (int i = 0; i < args.size(); i++) {
      EDTNode node = (EDTNode)args.elementAt(i);
      c.args.addElement(node.spawn(c, tree, i, node.irefnr, nt));
    }
    return c;
  }

  void setArgs(EDTNode se, EDTNode ctxt, int trefnr, int irefnr) {
    args.addElement(se);
    args.addElement(ctxt);
    this.trefnr = trefnr;
    this.irefnr = irefnr;
  }

  public Object inside(UI ui, int x, int y, int x0, int y0) {
    int cx = x0;
    EDTNode se = (EDTNode)args.elementAt(0);
    EDTNode ctxt = (EDTNode)args.elementAt(1);
    if (cx < x0)
      return null;
    if (Options.showcase.getState()) {
      cx += ui.normalfm.stringWidth(key);
      if (x <= cx)
	return this;
      if (x <= cx+se.width)
	return se.inside(ui, x, y, cx, y0);      
      cx += se.width + ui.normalfm.stringWidth(" m ");
      if (x <= cx)
	return null;
    }
    return ctxt.inside(ui, x, y, cx, y0);
  }

  public int paint(Graphics g, UI ui, int x0, int y0, int refnr, int trefnr, int irefnr) {
    layers = 0;
    EDTNode se = (EDTNode)args.elementAt(0);
    EDTNode ctxt = (EDTNode)args.elementAt(1);

    color = Color.black;
    if (this.trefnr >= 0) // Don't bother with cut-off-trees
      if (refnr == this.refnr) {
	if (this.irefnr == irefnr)
	  color = Color.red;
	else
	  color = Color.green;	      
      } else if (trefnr == this.trefnr)
	color = Color.blue;
    g.setColor(color);

    FontMetrics fm = g.getFontMetrics();
    int x = x0;

    if (Options.showcase.getState()) {
      g.drawString(key, x-ui.dx, y0-ui.dy+fm.getHeight());
      x += fm.stringWidth(key);
      x += se.paint(g, ui, x, y0, refnr, trefnr, irefnr); 
      int baseline = y0 + ui.normalfm.getHeight();
      int topline = baseline - ui.normalfm.getAscent()*5/6;
      width = ui.normalfm.charWidth('m');
      int space = ui.normalfm.charWidth(' ');
      g.setColor(Color.black);
      g.drawLine(x+space-ui.dx, (baseline+topline)/2-ui.dy, x+space+width-ui.dx, topline-ui.dy);
      g.drawLine(x+space-ui.dx, (baseline+topline)/2-ui.dy, x+space+width-ui.dx, baseline-ui.dy);
      g.drawLine(x+space+width-ui.dx, topline-ui.dy, x+space+width-ui.dx, baseline-ui.dy);
      x += space + width + space;
    }
    x += ctxt.paint(g, ui, x, y0, refnr, trefnr, irefnr); 

    width = x-x0;
    g.setFont(ui.normalfont);

    annotate(g, ui, x0, y0, refnr, trefnr, irefnr);

    /*
      underline(g, ui, x0, y0, width);
      if (this.refnr == trefnr && !ui.highlighting()) {
      ui.setHighlighting();
      // Paint background yellow, then paint our self again
      g.setColor(Color.yellow);
      g.fillRect(x0-ui.dx, y0-ui.dy+ui.normalfm.getDescent(), 
      width, ui.normalfm.getHeight());
      this.paint(g, ui, x0, y0, refnr, trefnr, irefnr);
      ui.clearHighlighting();	      
      }
    */
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

