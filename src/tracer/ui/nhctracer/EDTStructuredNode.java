package nhctracer;

import java.awt.*;
import java.util.Vector;

/**
 * An abstract node class for trail nodes that contain other trail nodes.
 */
public abstract class EDTStructuredNode extends EDTNode {
  /** The children nodes */
  Vector args;
  /** True if the node is contracted (shown as a box) */
  boolean contracted = false;
  
  /** Toggle contractness */
  public void contract() {
    contracted = !contracted;
  }

  /* Set the children to this node.
   * TO DO: evaluate and cache isTuple, isArithSeq, isInfix
   */
  public void setArgs(Vector args) {
    this.args = args;	
  }

  /** Update the level of layers in this node */
  public void childLayers(int layers) {
    if (layers > this.layers)
      this.layers = layers;
  }
    
  //    public boolean isInfix() {
  //        return (args != null && !(args.isEmpty()) && 
  //		args.elementAt(0) instanceof IdName &&
  //		((IdName)args.elementAt(0)).infix &&
  //		!((IdName)args.elementAt(0)).equals("[]"));
  //    }

  public boolean isTuple() {
    EDTNode head = (EDTNode)args.elementAt(0);
    return (head instanceof IdName &&
           ((IdName)head).tupleConsArity == args.size()-1);
  }
   
  public boolean isArithSeq() {
    EDTNode head = (EDTNode)args.elementAt(0);
    return (head instanceof IdName &&
           ((IdName)head).arithSeqArity == args.size()-1);
  }
   
  /* Checks whether this node needs parentheses.  This depends
   * both on the nature of the node itself and on the parent
   * context.
   */
  public boolean needParens() {
    if (isTuple()) return true;
    if (isArithSeq()) return false;
    if (parent==null || parent.isTuple() || parent.isArithSeq()
        || parent instanceof Case)
      return false;
    { EDTNode phead = (EDTNode)parent.args.elementAt(0);
    EDTNode mhead = (EDTNode)args.elementAt(0);
    if (phead instanceof IdName && phead.infix) {
      if (mhead instanceof IdName && mhead.infix) {
	int ppa = ((IdName)phead).pri;
	int mpa = ((IdName)mhead).pri;
	int pp = ppa / 4;
	int pa = ppa % 4;
	int mp = mpa / 4;
	int ma = mpa % 4;
	return (pp > mp || pp == mp &&
		(!(pa==ma&&(pa==1||pa==2)) || pa==index));
      } else return false;
    } else return true;
    }
  }
    
  public Object inside(UI ui, int x, int y, int x0, int y0) {
    int i, w;
    int cx;
    int spacew = ui.normalfm.charWidth(' ');
    EDTNode arg;
    String s;
    boolean isInfix = ((EDTNode)args.elementAt(0)).infix;
    boolean paren = needParens();

    if (contracted) {
      return (x >= x0 && x <= x0+width ? this : null);
    }

    if (x >= x0 && x <= x0+width) {
      if (args.isEmpty())
	return this;

      cx = x0;
      if (paren)
	cx += ui.normalfm.charWidth('(');
      if (x <= cx)
	return this;

      if (isTuple()) {
	int commaspacew = spacew + ui.normalfm.charWidth(',');
	for (i = 1; i < args.size(); i++) {
	  if (i > 1) {
	    cx += commaspacew;
	  }
	  if (x < cx) return this;
	  arg = (EDTNode)args.elementAt(i);
	  if (x <= cx+arg.width)
	    return arg.inside(ui, x, y, cx, y0);
	  cx += arg.width;		    
	}		
      } else if (isArithSeq()) {
        int kind = ((IdName)args.elementAt(0)).arithSeqKind;
	cx += ui.normalfm.charWidth('[');
	if (x <= cx) return this;
	arg = (EDTNode)args.elementAt(1);
	if (x <= cx+arg.width)
	    return arg.inside(ui, x, y, cx, y0);
	cx += arg.width;
	/* Sequence kinds are:
	 * 1 enumFrom, 2 enumFromThen, 3 enumFromTo, 4 enumFromThenTo
	 */
	switch (kind) {
	case 1: case 3:
	  cx += ui.normalfm.stringWidth("..");
	  if (x <= cx) return this;
	  if (kind==3) {
	    arg = (EDTNode)args.elementAt(2);
	    if (x <= cx+arg.width)
              return arg.inside(ui, x, y, cx, y0);
	  }
	  break;
	case 2: case 4:
          cx += ui.normalfm.stringWidth(", ");
	  if (x <= cx) return this;
	  arg = (EDTNode)args.elementAt(2);
	  if (x <= cx+arg.width)
	    return arg.inside(ui, x, y, cx, y0);
	  cx += arg.width + ui.normalfm.stringWidth("..");
	  if (x <= cx) return this;
	  if (kind==4) {
	    arg = (EDTNode)args.elementAt(3);
	    if (x <= cx+arg.width)
              return arg.inside(ui, x, y, cx, y0);
	  }
	  break;
        default:
	  System.err.println("Bad arithmetic sequence kind");
	  System.exit(1);
	}	  
      } else if (isInfix && args.size() == 3) {
	arg = (EDTNode)args.elementAt(1);
	if (x <= cx+arg.width)
	  return arg.inside(ui, x, y, cx, y0);
	cx += arg.width+spacew;
	if (x < cx) return this;
	arg = (EDTNode)args.elementAt(0);
	if (x <= cx+arg.width)
	  return arg.inside(ui, x, y, cx, y0);
	cx += arg.width+spacew;
	if (x < cx) return this;
	arg = (EDTNode)args.elementAt(2);
	if (x <= cx+arg.width)
	  return arg.inside(ui, x, y, cx, y0);
      } else {
	for (i = 0; i < args.size(); i++) {
	  arg = (EDTNode)args.elementAt(i);
	  if (i > 0) {
	    if (x <= cx+spacew) return this;
	    cx += spacew;
	  }
	  w = arg.width;
	  if (x <= cx+w)
	    return arg.inside(ui, x, y, cx, y0);
	  cx += w;
	}
      }
      return this;
    } else
      return null;
  }

  public int paint(Graphics g, UI ui, int x0, int y0,
                   int refnr, int trefnr, int irefnr, int drefnr) {
    int spacew = ui.normalfm.charWidth(' ');
    EDTNode arg;
    int i, x;
    boolean isInfix = ((EDTNode)args.elementAt(0)).infix;
    boolean paren = needParens();
    layers = 0;

    //this.x0 = x0;
    color = Color.black;
    if (drefnr > 0 && drefnr == this.drefnr) {
      color = Color.blue;
    } else if (refnr == this.refnr) {
      if (Options.highshare.getState()) color = Color.green;	      
    }
    g.setFont(ui.normalfont);
	
    if (contracted) {
      g.setColor(color);
      width = Symbols.drawPlaceholder(g, ui, x0, y0);
      underline(g, ui, x0, y0, width);
      return width;
    }

    x = x0;
    if (paren) {
      g.setColor(color);
      g.drawString("(", x0-ui.dx, y0-ui.dy+ui.normalfm.getHeight());
      x += ui.normalfm.charWidth('(');
    }
    if (isTuple()) {
      for (i = 1; i < args.size(); i++) {
	if (i > 1) {
	  g.setColor(color);
	  g.drawString(", ", x-ui.dx, 
		       y0-ui.dy+ui.normalfm.getHeight());
	  x += ui.normalfm.stringWidth(", ");
	}
	arg = (EDTNode)args.elementAt(i);
	x += arg.paint(g, ui, x, y0, refnr, trefnr, irefnr, drefnr);
      }
    } else if (isArithSeq()) {
      int kind = ((IdName)args.elementAt(0)).arithSeqKind;
      g.setColor(color);
      g.drawString("[", x-ui.dx, y0-ui.dy+ui.normalfm.getHeight());
      x += ui.normalfm.charWidth('[');
      arg = (EDTNode)args.elementAt(1);
      x += arg.paint(g, ui, x, y0, refnr, trefnr, irefnr, drefnr);
      /* Sequence kinds are:
       * 1 enumFrom, 2 enumFromThen, 3 enumFromTo, 4 enumFromThenTo
       */
      switch (kind) {
      case 1: case 3:
	g.setColor(color);
	g.drawString("..", x-ui.dx, y0-ui.dy+ui.normalfm.getHeight());
	x += ui.normalfm.stringWidth("..");
	if (kind==3) {
	  arg = (EDTNode)args.elementAt(2);
	  x += arg.paint(g, ui, x, y0, refnr, trefnr, irefnr, drefnr);
	}
	break;
      case 2: case 4:
	g.setColor(color);
	g.drawString(", ", x-ui.dx, y0-ui.dy+ui.normalfm.getHeight());
	x += ui.normalfm.stringWidth(", ");
	arg = (EDTNode)args.elementAt(2);
	x += arg.paint(g, ui, x, y0, refnr, trefnr, irefnr, drefnr);
	g.setColor(color);
	g.drawString("..", x-ui.dx, y0-ui.dy+ui.normalfm.getHeight());
	x += ui.normalfm.stringWidth("..");
	if (kind==4) {
	  arg = (EDTNode)args.elementAt(3);
	  x += arg.paint(g, ui, x, y0, refnr, trefnr, irefnr, drefnr);
	}
	break;
      default:
	System.err.println("Bad arithmetic sequence kind");
	System.exit(1);
      }
      g.setColor(color);
      g.drawString("]", x-ui.dx, y0-ui.dy+ui.normalfm.getHeight());
      x += ui.normalfm.charWidth(']');      	  
    } else if (isInfix && args.size() == 3) {
      String name = ((IdName)args.elementAt(0)).name;
      arg = (EDTNode)args.elementAt(1);
      x += arg.paint(g, ui, x, y0, refnr, trefnr, irefnr, drefnr) + spacew;
      arg = (EDTNode)args.elementAt(0);
      x += arg.paint(g, ui, x, y0, refnr, trefnr, irefnr, drefnr) + spacew;
      arg = (EDTNode)args.elementAt(2);
      x += arg.paint(g, ui, x, y0, refnr, trefnr, irefnr, drefnr); 
    } else {
      arg = (EDTNode)args.elementAt(0);
      x += arg.paint(g, ui, x, y0, refnr, trefnr, irefnr, drefnr);
      for (i = 1; i < args.size(); i++) {
	arg = (EDTNode)args.elementAt(i);
	x += spacew +
	     arg.paint(g, ui, x+spacew, y0, refnr, trefnr, irefnr, drefnr);
      }
    }
    width = x-x0;
    if (paren) {
      g.setColor(color);
      g.drawString(")", x-ui.dx, y0-ui.dy+ui.normalfm.getHeight());
      width += ui.normalfm.charWidth(')');
    }
    annotate(g, ui, x0, y0, refnr, trefnr, irefnr, drefnr);
    return width;
  }
}
