package nhctracer;

import java.awt.*;
import java.awt.event.*;

public class Option implements ItemListener {
  String name;
  boolean state;
  ActionListener al;

  public Option(String name, boolean state) {
    this.name = name;
    this.state = state;    
  }

  public void toggle() {
    state = !state;
    if (name.equals("Show source line numbers"))
      MainPanel.viewer.reload();
  }

  public void setState(boolean state) {
    this.state = state;
  }

  public boolean getState() {
    return state;
  }

  public void itemStateChanged(ItemEvent evt) {
    //System.err.print("Option '" + name + "' changed from " + state);
    toggle();
    //System.err.println(" to " + state);
  }
}



