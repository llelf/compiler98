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

  public CharList(EDTStructuredNode parent, TraceTree tree, int index, SourceRef sr, int refnr) {
    contracted = true;
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.sr = sr;
    this.refnr = refnr;
    trace = null;
    args = new Vector(3, 10);
  }

 public String getHelpText() {
   return "help text for CharList";
  }

  public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index, int irefnr, NodeTable nt) {
    CharList cl = new CharList(parent, tree, index);
    cl.sr = sr;
    cl.refnr = refnr;
    cl.trefnr = trefnr;
    cl.irefnr = irefnr;
    for (int i = 0; i < args.size(); i++) {
      EDTNode node = (EDTNode)args.elementAt(i);
      cl.args.addElement(node.spawn(cl, tree, i, node.irefnr, nt));
    }
    return cl;
  }
  
  public int paint(Graphics g, UI ui, int x0, int y0, int refnr, int trefnr, int irefnr) {
    int x;
    layers = 0;
    if (contracted) {
      boolean inMe = false;
      x = x0;
      g.setColor(Color.black);
      g.drawString("\"", x-ui.dx, y0-ui.dy+ui.normalfm.getHeight());
      x += ui.normalfm.charWidth('\"');
      EDTNode cons = this;
      EDTStructuredNode node;
      do {
	node = (EDTStructuredNode)cons;
	if (node.refnr == refnr) inMe = true;
	if (!(node.args.elementAt(1) instanceof IdName))
	  System.err.println(node.dump());
	IdName ch = (IdName)node.args.elementAt(1);
	if (ch.refnr == refnr) inMe = true;
	String s = ch.name.substring(1, ch.name.length()-1);
	g.drawString(s, x-ui.dx, y0-ui.dy+ui.normalfm.getHeight());
	x += ui.normalfm.stringWidth(s);
      } while ((cons = (EDTNode)node.args.elementAt(2)) instanceof EDTStructuredNode);
      g.drawString("\"", x-ui.dx, y0-ui.dy+ui.normalfm.getHeight());
      x += ui.normalfm.charWidth('\"');
      width = x - x0;
      underline(g, ui, x0, y0, width);
      if (inMe)
	boxify(g, ui, x0, y0, width);
    } else
      return super.paint(g, ui, x0, y0, refnr, trefnr, irefnr);
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
