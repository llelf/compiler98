package nhctracer;

import java.awt.*;
import java.util.Vector;

public class HString extends EDTNode {
  EDTStructuredNode root;
  String string;

  public HString(EDTStructuredNode root) {
    this.parent = root.parent;
    this.tree = root.tree;
    this.refnr = root.refnr;
    this.trefnr = root.trefnr;
    this.index = index;
    this.root = root;
  }

  public String getHelpText() {
    return "help text for HString";
  }

  static String getString(EDTNode cons) {
    String s = "\"";
    String ch;
    while (cons instanceof EDTStructuredNode &&
	   ((IdName)((EDTStructuredNode)cons).args.elementAt(0)).name.equals(":")) {
      ch = ((IdName)((EDTStructuredNode)cons).args.elementAt(1)).name;
      s += ch.substring(1, ch.length()-1);
      cons = (EDTNode)((EDTStructuredNode)cons).args.elementAt(2);
    }
    return s+"\"";
  }

  static boolean isCons(EDTNode node) {
    if (node != null && 
	node instanceof EDTStructuredNode &&
	!((EDTStructuredNode)node).args.isEmpty()) {
      EDTNode id = (EDTNode)((EDTStructuredNode)node).args.elementAt(0);
      if (id instanceof IdName &&
	  ((IdName)id).name.equals(":"))
	return true;
    }
    return false;
  }

  static boolean isChar(EDTNode node) {
    return (node != null && 
	    node instanceof IdName &&
	    ((IdName)node).name.charAt(0) == '\'');
  }      

  static boolean isNil(EDTNode node) {
    return (node != null && 
	    ((node instanceof IdName &&
	      ((IdName)node).name.charAt(0) == '[') ||
	     (node instanceof EDTStructuredNode &&
	      isNil((EDTNode)((EDTStructuredNode)node).args.elementAt(0)))));
  }      

  static EDTStructuredNode checkBackwards(EDTStructuredNode node) {
    if (isCons(node.parent) && isChar((EDTNode)node.parent.args.elementAt(1)))
      return checkBackwards(node.parent);
    else
      return node;
  }

  static boolean checkForward(EDTNode node) {
    if (isCons(node) &&
	isChar((EDTNode)((EDTStructuredNode)node).args.elementAt(1))) {
      return checkForward((EDTNode)((EDTStructuredNode)node).args.elementAt(2));
    } else {
      return isNil(node);
    }
  }

  public static boolean tryStringify(EDTNode node) {
    EDTStructuredNode root =  null;
    if (node instanceof IdName) 
      if (node.parent != null)
	node = node.parent;
      else
	return false;
    if (isCons(node) && isChar((EDTNode)((EDTStructuredNode)node).args.elementAt(1)))
      root = checkBackwards((EDTStructuredNode)node);
    if (root != null && checkForward((EDTStructuredNode)node)) {
      HString hs = new HString(root);
      hs.string = getString(root);
      root.parent.args.setElementAt(hs, root.index);
      return true;
    } else
      return false;	
  }

  public void consify() {
    root.parent.args.setElementAt(root, root.index);
  }

  public Object inside(UI ui, int x, int y, int x0, int y0) {
    if (x >= x0 && x <= x0+width) {
      return this;
    } else {
      return null;
    }
  }

  public int paint(Graphics g, UI ui, int x0, int y0, int refnr, int trefnr, int irefnr) {
    layers = 0;
    width = ui.normalfm.stringWidth(string);
    g.setColor(color);
    g.drawString(string, x0-ui.dx, y0-ui.dy+ui.normalfm.getHeight());
    return width;
  }

  public EDTNode spawn(EDTStructuredNode parent, TraceTree tree, int index, int irefnr, NodeTable nt) {
    return new HString(root);	
  }
  public String dump() {
    return "<hstr>" + dumprefs();
  }
}

