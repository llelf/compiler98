package nhctracer;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

public class ScriptMessage extends Dialog {
  TextArea ta = new TextArea(10, 60);
  Button ok = new Button("OK");
  Button cancel = new Button("Cancel");
  String message = null;

  ScriptMessage(Frame f) {
    super(f, "Message editor", true);
    setLayout(new BorderLayout());

    add(new Label("Enter message", Label.CENTER), BorderLayout.NORTH);
    add(ta, BorderLayout.CENTER);
    Panel pb = new Panel(new FlowLayout()); 
    pb.add(ok);
    pb.add(cancel);
    add(pb, BorderLayout.SOUTH);

    addMouseListener(new MouseAdapter() {
      public void mouseEntered(MouseEvent e) {
	ta.requestFocus();
      }
    });
    ok.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        message = ta.getText();
	setVisible(false);
      }
    });
    cancel.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
	message = null;
	setVisible(false);
      }
    });
    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
	message = null;
	setVisible(false);	
      }
    });
    pack();
    setVisible(true);
  }  
}
