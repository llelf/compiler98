package nhctracer;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

public class Help extends Frame {
  
  final int WIDTH = 475;
  final int HEIGHT = 350;
  UI ui;
  Frame me;
  
  public Help(String topic, UI ui) {
    super("Help with "+topic);
    this.ui = ui;
    me = this;
    Insets ins = getInsets();
    setSize(ins.left+WIDTH+ins.right, ins.top+HEIGHT+ins.bottom);
    this.add(new HelpCanvas(topic));
    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        me.dispose();
      }
    });
    setVisible(true);
  }
  
  class HelpCanvas extends Canvas {
    String topic;
    
    public HelpCanvas(String topic) {
      this.topic = topic;
      setSize(150,150);
      setBackground(Color.white);
    }
    
    public void paint(Graphics g) {
      super.paint(g);
      g.setFont(ui.normalfont);
      g.setColor(Color.black);
      int d = ui.normalfm.getHeight();
      int x = d/2;
      int y = d;      
      if (topic.compareTo("mouse clicks")==0) {
        int tab = ui.normalfm.stringWidth("middle") + 10;
	g.drawString("Clicking on a subexpression S means:", x, y);
	
	y += 2*d;
	g.drawString("left", x, y);
	  x += tab; 
	  g.drawString("expand/contract trace", x, y);
	  x += 10; y += d;
	  g.drawString("start new subtrace from parent redex of S", x, y);
	  y += d;
	  g.drawString("or remove existing subtrace", x, y);
        
	x = d/2; y += 2*d;
	g.drawString("middle", x, y);
	  x += tab;
	  g.drawString("expand/contract expression", x, y);
	  x += 10; y += d;
	  g.drawString("if S is a ", x, y);
	  x += ui.normalfm.stringWidth("if s is a ");
	  x += Symbols.drawPlaceholder(g,ui,x,y-d);
	  g.drawString(" place-holder, expand it", x, y);
	  x = d/2 + tab + 10; y += d;
	  g.drawString("otherwise contract S to a ", x, y);
	  x += ui.normalfm.stringWidth("otherwise contract S to a ");
	  x += Symbols.drawPlaceholder(g,ui,x,y-d);
        
	x = d/2; y += 2*d;
	g.drawString("right", x, y);
	  x += tab; 
	  g.drawString("show where S is formed in source code", x, y);        
	    x += 10; y += d;
	    g.drawString("or, if S is a name and shift is pressed,", x, y);
	    x -= 10; y += d;
	  g.drawString("show where S is defined in source code", x, y);
      
      } else if (topic.compareTo("expression highlights")==0) {
        g.drawString("If an expression E is highlighted:", x, y);
	
	y += 2*d;
	g.setColor(Color.black);
	g.drawString("in a red box", x, y);
	Symbols.drawSelected(g,ui,x,y-d,
	  ui.normalfm.stringWidth("in a red box"));
        g.setColor(Color.black);
	y+= d; x += 10;
	g.drawString("E is the currently selected expression", x,y);
	
        y += 2*d; x -= 10;
	ui.setHighlighting();
	g.setColor(Color.yellow);
	g.fillRect(x-ui.dx, y-d-ui.dy+ui.normalfm.getDescent(),
	           ui.normalfm.stringWidth("on a yellow background"), d);
	g.setColor(Color.black);
	g.drawString("on a yellow background", x, y);
	ui.clearHighlighting();
	y += d; x += 10;
	g.drawString("E is the parent of the selected expression", x, y);		
	
	y += 2*d; x -= 10;
	g.setColor(Color.blue);
	g.drawString("in blue", x, y);
	g.setColor(Color.black);
	y += d; x += 10;
	g.drawString(
	  "E and the selected expression have the same parent", x, y);
	
      } else if (topic.compareTo("special symbols")==0) {
        int tab = Symbols.maxWidth(ui) + 10;
	g.drawString("Non-haskell symbols can occur in expressions:", x, y);
	
	y += 2*d;
	Symbols.drawUndefined(g,ui,x,y-d);
	g.drawString("undefined or non-terminating expression", x+tab, y);
	y += d;
	g.drawString("(left-click to see what produced it)",x+tab,y);
		
	y += 2*d;
	Symbols.drawWithin(g,ui,x,y-d);
	g.drawString("links conditional or case to parent redex", x+tab, y);
        y += d;
	g.drawString("(read it as `within')", x+tab,y);
	 	
	y += 2*d;
	Symbols.drawPlaceholder(g,ui,x,y-d);
	g.drawString("placeholder for a contracted expression", x+tab, y);
	y += d;
	g.drawString("(middle-click to expand it)",x+tab, y);
	
	y += 2*d;
	Symbols.drawHidden(g,ui,x,y-d);
	g.drawString("unevaluated expression within a trusted module", x+tab, y);
	y += d;
	g.drawString("(left-click to see original trusted application)",
	             x+tab,y);
	
	y += 2*d;
	Symbols.drawUnavailable(g,ui,x,y-d);
	g.drawString("an untraceable expression", x+tab, y);
	y += d;
	g.drawString("(rare)", x+tab, y);
      }
    }
  }
  
}
