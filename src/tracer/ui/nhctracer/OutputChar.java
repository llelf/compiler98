package nhctracer;

import java.awt.*;
import java.util.Vector;

public class OutputChar extends Object {
    char ch;
    String s;
    int refnr;
    int width;

    public OutputChar(char ch, int refnr) {
        this.ch = ch;
	s = String.valueOf(ch);
	this.refnr = refnr;
    }

    public int paint(Graphics g, UI ui, int x0, int y0, int refnr) {
        width = ui.normalfm.stringWidth(s);
	if (this.refnr == refnr)
	    g.setColor(Color.blue);
	else
	    g.setColor(Color.black);
  
	g.drawString(s, x0-ui.dx, y0-ui.dy+ui.normalfm.getHeight());
	return width;
    }
}

