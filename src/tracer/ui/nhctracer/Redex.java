package nhctracer;

import java.awt.*;
import java.util.Vector;

public class Redex extends EDTStructuredNode {
  public Redex(EDTStructuredNode parent, TraceTree tree, int index) {
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.trace = null;
    args = new Vector(3, 10);
  }

  public Redex(EDTStructuredNode parent, TraceTree tree, int index, SourceRef sr, int refnr) {
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.sr = sr;
    this.refnr = refnr;
    trace = null;
    args = new Vector(3, 10);
  }

  public String getHelpText() {
    return "help text for Redex";
  }

  public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index, int irefnr, NodeTable nt) {
    Redex redex = new Redex(parent, tree, index);
    redex.sr = sr;
    redex.refnr = refnr;

    redex.trefnr = trefnr;
    redex.irefnr = irefnr;
    for (int i = 0; i < args.size(); i++) {
      EDTNode node = (EDTNode)args.elementAt(i);
      // Should it be redex.tree here?
      redex.args.addElement(node.spawn(redex, tree, i, node.irefnr, nt));
    }
    return redex;    
  }

  public String dump() {
    String res = "(r " + dumprefs();
    for (int i = 0; i < args.size(); i++) {
      res += " " + ((EDTNode)args.elementAt(i)).dump();
    }
    return res + ")";
  }

}
