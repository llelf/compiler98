package nhctracer;

import java.awt.*;
import java.util.Vector;
import java.applet.*;

public class HText extends Applet {
    HCanvas hc;

    public void init() {
	setLayout(new BorderLayout());

	/*
	setBackground(Color.red);
	setForeground(Color.white);
	*/

	hc = new HCanvas();
	add("Center", hc);
   }
}

class HCanvas extends Canvas {
    FontMetrics fm;
    Color mbgc, bgc, fgc, lc;
    Font font;
    String message = "Haskell debugger";
    Rectangle msgRect;
    int msgX = 100;
    int msgY = 100;
    int delta = 10;
    boolean buttonPressed;
    boolean clearNeeded = true;
    CallChain chain;
    EDTNode lastNode = null;
    public static final int START_Y = 32;

    public HCanvas() {
	super();

	CallChain cc1, cc2;
	EDTNode en1, en2;
	
	font = new Font("Helvetica", Font.BOLD, 24);
	fm = getFontMetrics(font);
	bgc = Color.white;
	fgc = Color.red;
	lc = Color.black;
	mbgc = Color.cyan;
	buttonPressed = false;
	setBackground(Color.lightGray);

	chain = new CallChain(null, 1); 
	en1 = new Redex(chain, 0, "foo");
	new Producer(en1, 1, "fie", "1");
	new Producer(en1, 2, "bar", "3");
	en1 = new Redex(en1, 3, "kalle");
	new Producer(en1, 1, "arg", "2");
	cc1 = new CallChain(chain, 1);
	en1 = new Redex(cc1, 0, "nisse");
	new Redex(en1, 1, "arne");
	cc1 = new CallChain(cc1, 1);
	en1 = new Redex(cc1, 0, "ture");
	new Redex(en1, 1, "apa");
	cc1 = new CallChain(chain, 2);
	en1 = new Redex(cc1, 0, "stellan");
	new Redex(en1, 1, "gurka");
    }

    public void moveText(int dx, int dy) {
	msgX += dx;
	msgY += dy;
	repaint();
    }

    boolean inside(int x, int y, Rectangle r) {
	return (x <= r.x+r.width && x >= r.x && y <= r.y+r.height && y > r.y);
    } 

    public boolean mouseEnter(Event evt, int x, int y) {
if (false) {
	lc = Color.yellow;
	repaint();
}
	return true;
    }

    public boolean mouseExit(Event evt, int x, int y) {
if (false) {
	lc = Color.black;
	repaint();
}
	return true;
    }

    public boolean mouseMove(Event evt, int x, int y) {
	EDTNode node = chain.insideNode(this, x, y, START_Y);
	
	if (node == null) {	
	    if (lastNode != null) {
		lastNode.color = Color.red;
		lastNode = null;
		repaint();
	    }
	} else {
	    if (node != lastNode) {
		if  (lastNode != null)
		    lastNode.color = Color.red;
		node.color = Color.blue;
	    	lastNode = node;
	    	repaint();
	    }
	}
	return true;
    }

    public boolean mouseDown(Event evt, int x, int y) {
	EDTNode node = chain.insideNode(this, x, y, START_Y);
	if (node != null) {
	    System.err.println(node.path());
	    return true;
	}
	return true;
    }

    public boolean mouseUp(Event evt, int x, int y) {
if (false) {
	if (buttonPressed) {
	    buttonPressed = false;
	    if (inside(x, y, msgRect))
		return mouseEnter(evt, x, y);
	    else
		return mouseExit(evt, x, y);
	} else
	    return false;
}
	return false;
    }

    public void paint(Graphics g) {
	Dimension size = this.size();

	if (false && clearNeeded) {
	    g.setColor(bgc);
	    g.fillRect(0, 0, size.width, size.height);
	    clearNeeded = false;
	}

	/* g.setColor(Color.red); */
	chain.paint(g, this, START_Y);
    }
}

abstract class EDTNode extends Object {
    public static final int REDEX = 1;
    public static final int PRODUCER = 2;
    public static final int UNEVAL = 3;
    public static final int BOTTOM = 4;
    public static final int NCHILDREN = 10;

    String sourceFile;
    int sourceCol, sourceLine;
    EDTNode parent;
    CallChain cparent;
    EDTNode children[];
    int nchildren = 0;
    Color color = Color.red;
    int index;
    int type;
    int x0, y0;
    int width;

    abstract public String show();
    abstract public int paint(Graphics g, HCanvas hc, int x0_, int y0);

    public void addChild(EDTNode child) {
	children[nchildren++] = child;			
    }

    public EDTNode insideNode(HCanvas hc, int x, int y, int y0) {
	int i, cx;
	EDTNode result = null;
	if (x >= x0 && x <= x0+width) {
	    for(i=0; i < nchildren; i++) {
		if (result == null)
		    result = children[i].insideNode(hc, x, y, y0);
	    }
	    if (result == null)
		return this;
	    else
		return result;
	} else
	    return null;
    }

    public String path() {
	if (parent == null)
	    return cparent.path() + "\nEDT-path:\n";
	else
	    return parent.path() + " " + index;
    }
}

class Redex extends EDTNode {
    String funName;
    EDTNode argRedex[][];

    void init(CallChain cp, EDTNode p, int i, String fn) {
	cparent = cp;
	parent = p;
	index = i;
	funName = fn;	
	argRedex = null;
	children = new EDTNode[NCHILDREN];
	if (parent == null)
	    cparent.setNode(this);
	else
	    parent.addChild(this);
    }

    public Redex(CallChain cp, int index_, String funName_) {
	init(cp, null, index_, funName_);
    }

    public Redex(EDTNode parent_, int index_, String funName_) {
	init(parent_.cparent, parent_, index_, funName_);
    }

    public int paint(Graphics g, HCanvas hc, int x0_, int y0) {
	String s;
	int spacew = hc.fm.stringWidth(" ");
	int i, x;

	x0 = x0_;
	g.setColor(color);
	g.setFont(hc.font);
	
	if (nchildren == 0) {
	   g.drawString(funName, x0, y0);
	   width = hc.fm.stringWidth(funName);
	} else {
	    if (parent == null)
		s = funName;
	    else
		s = "(" + funName;
	    g.drawString(s, x0, y0);
	    x = x0+hc.fm.stringWidth(s);
	    for (i = 0; i < nchildren; i++) {
	        x += spacew + children[i].paint(g, hc, x+spacew, y0);	    
	    }
	    width = x-x0;
	    if (parent != null) {
	        g.setColor(color);
	        g.drawString(")", x, y0);
		width += hc.fm.stringWidth(")");
	    }
	}
	return width;
    }

    public String show() {
	return "(" + funName + " ... args)";
    }
}

class Producer extends EDTNode {
    String prodName;
    String value;

    void init(CallChain cp, EDTNode p, int i, String pn, String v) {
	cparent = cp;
	parent = p;
	index = i;
	prodName = pn;
	value = v;
	children = new EDTNode[NCHILDREN];
	if (parent == null)
	    cparent.setNode(this);
	else
	    parent.addChild(this);
    }

    public Producer(CallChain cp, int index_, String prodName_, String value_){
	init(cp, null, index_, prodName_, value_);
    }

    public Producer(EDTNode parent_, int index_, String prodName_, String value_) {
	init(parent_.cparent, parent_, index_, prodName_, value_);
    }

    public int paint(Graphics g, HCanvas hc, int x0_, int y0) {
	String s;
	int spacew = hc.fm.stringWidth(" ");
	int i, x;

	x0 = x0_;
	g.setColor(color);
	g.setFont(hc.font);
	
	if (nchildren == 0) {
	   g.drawString(prodName, x0, y0);
	   width = hc.fm.stringWidth(prodName);
	} else {
	    if (parent == null)
		s = prodName;
	    else
		s = "(" + prodName;
	    g.drawString(s, x0, y0);
	    x = x0+hc.fm.stringWidth(s);
	    for (i = 0; i < nchildren; i++) {
	        x += spacew + children[i].paint(g, hc, x+spacew, y0);	    
	    }
	    width = x-x0;
	    if (parent != null) {
	        g.setColor(color);
	        g.drawString(")", x, y0);
		width += hc.fm.stringWidth(")");
	    }
	}
	return width;
    }

    public String show() {
	return value;
    }
}

class CallChain extends Object {
    EDTNode node;
    CallChain parent;
    Vector children;
    Vector ccolors;
    int nchildren = 0;
    int index;
    int x0 = 0;
    int height;
    boolean hidden = false;
    public static final Color apa[] = {Color.blue, Color.red, Color.green};
    public static final int LINE_HEIGHT = 30;
    public static final int INDENT = 32;
    public static final int NCHILDREN = 10;
    
    public CallChain(CallChain parent_, int index_) {
	parent = parent_;
	index = index_;
	children = new Vector(3, 10);
	ccolors = new Vector(3, 10);
	if (parent == null) {
	    x0 = 8;
	} else {
	    parent.addChild(this);
	}
    }

    public void setNode(EDTNode node_) {
	node = node_;
    }

    public int addChild(CallChain child) {
	children.addElement(child);	
	return children.elementCount;
    }

    public EDTNode insideNode(HCanvas hc, int x, int y, int y0) {
	int i, cy;
	int fh = hc.fm.getHeight();
	EDTNode result = null;
	if (y >= y0-fh && y <= y0+height-fh) {
	    if (y <= y0) {			/* in the node */
		return node.insideNode(hc, x, y, y0);
	    } else {				/* in a child */
		cy = y0+hc.fm.getHeight();
		for (i=0; i < children.elementCount; i++) {
		    if (result == null)
		        result = children.elementAt(i).insideNode(hc, x, y, cy);
		    cy += children[i].height;
		}
		return result;
	    }
	} else
	    return null;
    }

    public int paint(Graphics g, HCanvas hc, int y0) {
	int i, y;

	if (x0 == 0) {
	    x0 = parent.x0 + INDENT;
	}
	
	node.paint(g, hc, x0, y0);

	y = y0 + hc.fm.getHeight();
	for (i = 0; i < NCHILDREN; i++)
	    if (children[i] != null)
		y += children[i].paint(g, hc, y);
	height = y-y0;
	return height;
    }

    public String path() {
	if (parent == null)
	    return "Call chain path:\n";
	else
	    return parent.path() + " " + index;
    }
}

class EDTNode extends Object {
    String sourceFile;
    int sourceCol, sourceLine;
    EDTNode parent;
    CallTree tree;
    CallChain chain;
    EDTNode children[];
    int nchildren = 0;
    Color color = Color.red;
    int index;
    int type;
    int x0, y0;
    int width;
}

class CallTree extends Object {
    EDTNode node;
    CallChain parent;
    Vector children;
    Vector ccolors;
    int nchildren = 0;
    int index;
    int x0 = 0;
    int height;
    boolean hidden = false;
    public static final Color apa[] = {Color.blue, Color.red, Color.green};
    public static final int LINE_HEIGHT = 30;
    public static final int INDENT = 32;
    public static final int NCHILDREN = 10;
}

class CallChain extends Object {
    Color color;
    Vector nodes;
    boolean hidden;
    EDTNode parent;

    public CallChain(EDTNode parent_) {
	nodes = new Vector(3, 10);
	hidden = false;
	parent = parent_;
    }
}
