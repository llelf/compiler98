package nhctracer;

import java.awt.*;

public class IdName extends EDTNode {
  String module;
  String name;
  int defpos, pri, isIdentifier;
  int tupleConsArity;/* if this is a tuple constructor, its arity, or -1 */
  int arithSeqArity; /* if this is an enumFrom... function, its arity, or -1 */
  int arithSeqKind;  /* ditto its kind:
      * 1 enumFrom, 2 enumFromThen, 3 enumFromTo, 4 enumFromThenTo
      */

  public IdName(EDTStructuredNode parent, TraceTree tree, int index) {
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.trace = null;
  }

  public IdName(EDTStructuredNode parent, TraceTree tree, int index,
                SourceRef sr, int refnr, String module, String name,
	        int isIdent,int defpos, int pri) {
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.sr = sr;
    this.refnr = refnr;
    this.trace = null;
	
    this.module = module;
    this.name = name;
    this.isIdentifier = isIdent;
    this.defpos = defpos;
    this.pri = pri;

    int c;
    if (name.equals("")) c=' '; else c = this.name.charAt(0);
    infix = !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
	      (c >= '0' && c <= '9') || c == '(' || c == '[' || 
	      c == '\'' || c == ',' || c == '_' || this.name.equals("\\"));
    if (c==',') tupleConsArity = name.length()+1;
    else tupleConsArity = -1;
    if (c=='e') {
      if (name.equals("enumFrom")) {
        arithSeqArity = 1; arithSeqKind = 1;
      } else if (name.equals("enumFromThen")) {
        arithSeqArity = 2; arithSeqKind = 2;
      } else if (name.equals("enumFromTo")) {
        arithSeqArity = 2; arithSeqKind = 3;
      } else if (name.equals("enumFromThenTo")) {
        arithSeqArity = 3; arithSeqKind = 4;
      } else {
        arithSeqArity = -1; arithSeqKind = -1;
      }
    } else { arithSeqArity = -1; arithSeqKind = -1; }
  }

  public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index,
                       int irefnr, int drefnr, NodeTable nt) {
    IdName idName = new IdName(parent, tree, index);
    //System.err.println("spawning " + this + "(" + this.name + ") into " + idName);
    idName.sr = sr;
    idName.refnr = refnr;
    idName.trefnr = trefnr;
    idName.irefnr = irefnr;
    idName.drefnr = drefnr;
    idName.name = name;
    idName.isIdentifier = isIdentifier;
    idName.module = module;
    idName.defpos = defpos;
    idName.pri = pri;
    idName.tupleConsArity = tupleConsArity;
    idName.arithSeqArity = arithSeqArity;
    idName.arithSeqKind = arithSeqKind;
    idName.infix = infix;
    return idName;
  }

  public Object inside(UI ui, int x, int y, int x0, int y0) {
    if (x >= x0 && x <= x0+width) {
      return this;
    } else {
      return null;
    }
  }

  public int paint(Graphics g, UI ui, int x0, int y0,
                   int refnr, int trefnr, int irefnr, int drefnr) {
    boolean parenthesize = false;
    layers = 0;
    color = Color.black;
    if (drefnr > 0 && drefnr == this.drefnr) {
      color = Color.blue;
    } else if (refnr == this.refnr) {
      if (Options.highshare.getState()) color = Color.green;	      
    }
    g.setColor(color);
    FontMetrics fm = g.getFontMetrics();
    int x = x0;
    if (name.equals("\\")
    || infix &&
       (index > 0 || (parent != null &&
                      parent.args.size() != 3))
    || tupleConsArity > 0 &&
       (index > 0 || (parent != null &&
                      parent.args.size() != tupleConsArity+1))) {
      parenthesize = true;
      g.drawString("(", x-ui.dx, y0-ui.dy+fm.getHeight());
      x += fm.stringWidth("(");
    }
    if (name.equals("[]")) {
      g.drawString("[ ]", x-ui.dx, y0-ui.dy+fm.getHeight());
      x += fm.stringWidth("[ ]");
    } else if (name.equals("\\")) {
      g.drawString("\\\u22EF", x-ui.dx, y0-ui.dy+fm.getHeight());
      x += fm.stringWidth("\\\u22EF");
    } else {
      g.drawString(name, x-ui.dx, y0-ui.dy+fm.getHeight());
      x += fm.stringWidth(name);
    }
    if (parenthesize) {
      g.drawString(")", x-ui.dx, y0-ui.dy+fm.getHeight());
      x += fm.stringWidth(")");
    }
    width = x-x0;
    g.setFont(ui.normalfont);

    annotate(g, ui, x0, y0, refnr, trefnr, irefnr, drefnr);
    return width;
  }
  public String dump() {
    return "<N: " + name + ">" + dumprefs();
  }
}

