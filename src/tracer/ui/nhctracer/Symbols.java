package nhctracer;

import java.awt.*;

public class Symbols {

  public static int drawPlaceholder(Graphics g, UI ui, int x, int y) {
    int width = ui.normalfm.charWidth('m');
    int w = width * 5 / 6;
    int baseline = y + ui.normalfm.getHeight();
    int topline = baseline - ui.normalfm.getAscent()*5/6;
    g.drawRect(x-ui.dx, topline-ui.dy, w, baseline-topline);
    return width;
  }

  public static int drawHidden(Graphics g, UI ui, int x0, int y0) {
    int width = ui.normalfm.charWidth('m');
    int w = width * 5 / 6;
    int baseline = y0 + ui.normalfm.getHeight();
    int topline = baseline - ui.normalfm.getAscent()*5/6;
    g.drawRect(x0-ui.dx, topline-ui.dy, w, baseline-topline);
    g.drawLine(x0-ui.dx, baseline-ui.dy, x0+w-ui.dx, topline-ui.dy);
    return width;
  }
  
  public static int drawUnavailable(Graphics g, UI ui, int x0, int y0) {
    int width = ui.normalfm.charWidth('m');
    int w = width * 5 / 6;
    int baseline = y0 + ui.normalfm.getHeight();
    int topline = baseline - ui.normalfm.getAscent()*5/6;
    g.drawRect(x0-ui.dx, topline-ui.dy, w, baseline-topline);
    g.drawLine(x0-ui.dx, baseline-ui.dy, x0+w-ui.dx, topline-ui.dy);
    g.drawLine(x0-ui.dx, topline-ui.dy, x0+w-ui.dx, baseline-ui.dy);
    return width;
  }
  
  public static int drawWithin(Graphics g, UI ui, int x0, int y0) {
    int width = ui.normalfm.charWidth('m');
    int baseline = y0 + ui.normalfm.getHeight();
    int topline = baseline - ui.normalfm.getAscent()*5/6;
    g.drawLine(
      x0-ui.dx, (baseline+topline)/2-ui.dy,
      x0+width-ui.dx, topline-ui.dy);
    g.drawLine(
      x0-ui.dx, (baseline+topline)/2-ui.dy,
      x0+width-ui.dx, baseline-ui.dy);
    g.drawLine(
      x0+width-ui.dx, topline-ui.dy,
      x0+width-ui.dx, baseline-ui.dy);
    return width;
  }
  
  public static int drawUndefined(Graphics g, UI ui, int x0, int y0) { 
    int width = ui.normalfm.charWidth('m');
    int w = width * 5 / 6;
    int baseline = y0 + ui.normalfm.getHeight();
    int topline = baseline - ui.normalfm.getAscent()*5/6;
    g.drawLine(x0-ui.dx, baseline-ui.dy, x0+w-ui.dx, baseline-ui.dy);
    g.drawLine(x0-ui.dx, baseline-1-ui.dy, x0+w-ui.dx, baseline-1-ui.dy);
    g.drawLine(x0+w/2-ui.dx, baseline-ui.dy, x0+w/2-ui.dx, topline-ui.dy);
    g.drawLine(x0+w/2+1-ui.dx, baseline-ui.dy, x0+w/2+1-ui.dx, topline-ui.dy);
    return width;
  }
  
  public static int maxWidth(UI ui) {
    return ui.normalfm.charWidth('m');
  }

  public static void drawSelected(Graphics g, UI ui,
                                  int x0, int y0, int width) {
    g.setColor(Color.red);
    int baseline = y0 + 5 + ui.normalfm.getHeight();
    int topline = y0+3; //baseline - ui.normalfm.getAscent();
    g.drawRect(x0-3-ui.dx, topline+1-ui.dy, 
	       width+6, baseline-topline-2);	    
    g.drawRect(x0-2-ui.dx, topline-ui.dy, 
	       width+4, baseline-topline);	          
  }

}


