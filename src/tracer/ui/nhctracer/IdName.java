package nhctracer;

import java.awt.*;

public class IdName extends EDTNode {
  String module;
  String name;
  int defpos, pri;
  int tupleConsArity; /* if this is a tuple constructor, its arity, else -1 */

  public IdName(EDTStructuredNode parent, TraceTree tree, int index) {
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.trace = null;
  }

  public IdName(EDTStructuredNode parent, TraceTree tree, int index,
                SourceRef sr, int refnr, String module, String name,
	        int defpos, int pri) {
    this.parent = parent;
    this.tree = tree;
    this.index = index;
    this.sr = sr;
    this.refnr = refnr;
    this.trace = null;
	
    this.module = module;
    this.name = name;
    this.defpos = defpos;
    this.pri = pri;

    int c = this.name.charAt(0);
    infix = !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
	      (c >= '0' && c <= '9') || c == '(' || c == '[' || 
	      c == '\'' || c == ',' || this.name == "\\");
    if (c==',') tupleConsArity = name.length()+1;
    else tupleConsArity = -1;
  }

  public String getHelpText() {
    return "help text for IdName";
  }

  public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index, int irefnr, NodeTable nt) {
    IdName idName = new IdName(parent, tree, index);
    //System.err.println("spawning " + this + "(" + this.name + ") into " + idName);
    idName.sr = sr;
    idName.refnr = refnr;
    idName.trefnr = trefnr;
    idName.irefnr = irefnr;
    idName.name = name;
    idName.module = module;
    idName.defpos = defpos;
    idName.pri = pri;
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

  public int paint(Graphics g, UI ui, int x0, int y0, int refnr, int trefnr, int irefnr) {
    boolean parenthesize = false;
    layers = 0;

    color = Color.black;
    if (this.trefnr >= 0) // Don't bother with cut-off-trees
      if (refnr == this.refnr) {
	if (this.irefnr == irefnr)
	  color = Color.black;
	else
	  color = Color.green;	      
      } else if (trefnr == this.trefnr)
	color = Color.blue;
    g.setColor(color);

    if (false) { // Non-clickable
      g.setFont(ui.boldfont);
    }
    FontMetrics fm = g.getFontMetrics();
    int x = x0;
    if (infix &&
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

    annotate(g, ui, x0, y0, refnr, trefnr, irefnr);
    return width;
  }
  public String dump() {
    return "<N: " + name + ">" + dumprefs();
  }
}

