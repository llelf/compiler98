package nhctracer;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

public class StringDialog extends Dialog {
  
  UI ui;
  Dialog me;
  Choice c;
  static int endLengths = 0;
  
  public StringDialog(Frame f, UI ui) {
    super(f, "Max length of strings displayed in full", true);
           
    c = new Choice();
    c.add("2 chars.");
    c.add("4 chars.");
    c.add("8 chars.");
    c.add("16 chars.");
    c.add("32 chars.");
    c.add("64 chars.");
    c.add("no limit");
    c.select(6);
    
    Panel p = new Panel();
    p.add(c);
    add(p);
   
    setSize(400,100);
    
    c.addItemListener(new ItemListener() {
      public void itemStateChanged(ItemEvent ie) {
        int i = c.getSelectedIndex();
	endLengths = (i < 6 ? (int)Math.round(Math.pow(2,i)) : 0);
      }
    });
        
    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        setVisible(false);
      }
    });
   
    setVisible(true);
  }
    
}
