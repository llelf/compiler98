package nhctracer;
import java.awt.*;
import java.applet.*;

public class XXX extends Frame {
  CanvasPanel canvas;

  public XXX() {
    canvas = new CanvasPanel();
    Panel p = new Panel();
    p.setLayout(new BorderLayout(0,0));
    p.add("Center", canvas);
    this.setLayout(new BorderLayout(0,0));
    add("Center", p);
    resize(300, 300);
    repaint();
    show();
  }

  public void update(Graphics g) {
    System.err.println("Update!");
  }

  public void paint(Graphics g) {
    System.err.println("Painting!");
    canvas.setForeground(Color.blue);
    Graphics cg = canvas.getGraphics();
    cg.drawLine(0, 0, 300, 300);
  }

  //  public boolean handleEvent(Event e) {
  //    System.err.println("Event(Enter): " +Event.GOT_FOCUS+ " " + Event.LOST_FOCUS + " " + Event.WINDOW_EXPOSE + " " +Event.WINDOW_MOVED + " "+e);	
  //    return false;
  //  }

  public static void main(String[] argv) {
    new XXX();
  }
}

class CanvasPanel extends Panel {
  public CanvasPanel() {
  }

  public void update(Graphics g) {
    System.err.println("Update!");
  }

  public void paint(Graphics g) {
    System.err.println("Painting!");
    setForeground(Color.blue);
    g.drawLine(0, 0, 300, 300);
  }

  public boolean handleEvent(Event e) {
    System.err.println("Event(Enter): " +Event.GOT_FOCUS+ " " + Event.LOST_FOCUS + " " + Event.WINDOW_EXPOSE + " " +Event.WINDOW_MOVED + " "+e);	
    return false;
  }
}
