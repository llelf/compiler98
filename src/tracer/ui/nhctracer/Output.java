package nhctracer;

import java.awt.*;
import java.util.Vector;

public class Output extends Object {
    Vector lines;
    Vector curline;
    int width, height;
    int lineno, colno;

    public Output() {
        lines = new Vector(50, 50);
	curline = null;
    }

    public void addChar(char ch, int refnr) {
        OutputChar oc = new OutputChar(ch, refnr);
	if (curline == null) {
	    curline = new Vector(10, 10);
	    lines.addElement(curline);	  
	}
	curline.addElement(oc);
	if (ch == '\n') {
	    curline = new Vector(10, 10);
	    lines.addElement(curline);
	} 
    }

    public Object inside(UI ui, int x, int y, int x0, int y0) {
        lineno = (y-y0) / ui.normalfm.getHeight();
	//System.err.println("lineno="+lineno+" y="+y+" y0="+y0+" ui.dy="+ui.dy);
	if (lineno < lines.size()) {
	    Vector line = (Vector)lines.elementAt(lineno);
	    int cx = x0;
	    for (int i = 0; i < line.size(); i++) {
		OutputChar oc = (OutputChar)line.elementAt(i);
		cx += oc.width;
		if (x < cx) {
		    colno = i;
		    return oc;
		}
	    }
	}
	return null;
    }

    public Dimension paint(Graphics g, UI ui, int x0, int y0, int refnr) {
	int w, h = y0;
	//System.err.println("Output.paint");
	g.setColor(Color.black);
	width = 0;
	height = 0;
	for (int i = 0; i < lines.size(); i++) {
	    Vector line = (Vector)lines.elementAt(i);	   
	    //System.err.println("Line "+(i+1)+" has "+line.size()+" chars.");
	    w = 0;
	    for (int j = 0; j < line.size(); j++) {
		w += ((OutputChar)line.elementAt(j)).paint(g, ui, x0 + w, y0+height, refnr);
	    }
	    if (w > width)
		width = w;
	    height += ui.normalfm.getHeight();
	}
	return new Dimension(width, height);
    }

}
