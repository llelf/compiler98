package nhctracer;

import java.awt.*;
import java.util.Vector;

public class CharList extends EDTStructuredNode {
  public CharList(EDTStructuredNode parent, TraceTree tree, int index) {
    contracted = true;
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.trace = null;
    args = new Vector(3, 10);
  }

  public CharList(EDTStructuredNode parent, TraceTree tree, int index,
                  SourceRef sr, int refnr) {
    contracted = true;
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.sr = sr;
    this.refnr = refnr;
    trace = null;
    args = new Vector(3, 10);
  }

  public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index,
                       int irefnr, int drefnr, NodeTable nt) {
    CharList cl = new CharList(parent, tree, index);
    cl.sr = sr;
    cl.refnr = refnr;
    cl.trefnr = trefnr;
    cl.irefnr = irefnr;
    cl.drefnr = drefnr;
    for (int i = 0; i < args.size(); i++) {
      EDTNode node = (EDTNode)args.elementAt(i);
      cl.args.addElement(
        node.spawn(cl, tree, i, node.irefnr, node.drefnr, nt));
    }
    return cl;
  }
  
  public int paint(Graphics g, UI ui, int x0, int y0,
                   int refnr, int trefnr, int irefnr, int drefnr) {
    int x;
    layers = 0;
    if (contracted) {
      x = x0;
      g.setColor(Color.black);
      g.drawString("\"", x-ui.dx, y0-ui.dy+ui.normalfm.getHeight());
      x += ui.normalfm.charWidth('\"');
      EDTNode cons = this;
      EDTStructuredNode node;
      do {
	node = (EDTStructuredNode)cons;
	IdName ch = (IdName)node.args.elementAt(1);
	String s = ch.name.substring(1, ch.name.length()-1);
	g.drawString(s, x-ui.dx, y0-ui.dy+ui.normalfm.getHeight());
	x += ui.normalfm.stringWidth(s);
      } while ((cons = (EDTNode)node.args.elementAt(2))
                instanceof EDTStructuredNode);
      g.drawString("\"", x-ui.dx, y0-ui.dy+ui.normalfm.getHeight());
      x += ui.normalfm.charWidth('\"');
      width = x - x0;
      annotate(g, ui, x0, y0, refnr, trefnr, irefnr, drefnr);
    } else
      return super.paint(g, ui, x0, y0, refnr, trefnr, irefnr, drefnr);
    return width;
  }
  public String dump() {
    String res = "(str " + dumprefs();
    for (int i = 0; i < args.size(); i++) {
      res += " " + ((EDTNode)args.elementAt(i)).dump();
    }
    return res + ")";
  }
}
