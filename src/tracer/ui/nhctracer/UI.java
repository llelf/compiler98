package nhctracer;

import java.awt.FontMetrics;
import java.awt.Font;
import java.awt.Color;

public class UI extends Object {
  FontMetrics normalfm;
  FontMetrics boldfm;
  Font normalfont;
  Font boldfont;
  int dx, dy;
  boolean hl;
  public static final Color colors[] = 
  {Color.green, Color.magenta, Color.blue, Color.cyan, Color.yellow, 
   Color.red, Color.black, Color.gray};
  int color = -1;

  public UI() {
    hl = false;
    dx = 0;
    dy = 0;
  }

  public Color getColor() {
    color = (color+1) % colors.length;
    return colors[color];
  }

  public void resetColors() {
    color = -1;
  }

  public boolean highlighting() {
    return hl;
  }
  public void setHighlighting() {
    hl = true;
  }
  public void clearHighlighting() {
    hl = false;
  }
}

