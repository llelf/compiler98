package nhctracer;

import java.awt.*;
import java.awt.event.*;

public class ModalMessage extends Dialog {
  Font font = new Font("Serif", Font.BOLD, 16);
  Button ok = new Button("OK");

  ModalMessage(Frame f, String msg) {
    super(f, "Message", true);
    setFont(font);
    setLayout(new BorderLayout());
    Label l = new Label(msg, Label.CENTER);
    add(l, BorderLayout.NORTH);

    Panel bp = new Panel(new FlowLayout());
    bp.add(ok);
    add(bp, BorderLayout.SOUTH);

    ok.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
	setVisible(false);
      }
    });
    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
	setVisible(false);	
      }
    });
    addMouseListener(new MouseAdapter() {
      public void mouseEntered(MouseEvent evt) {
	ok.requestFocus();
      }      
    });
    ok.addKeyListener(new KeyAdapter() {
      public void keyPressed(KeyEvent evt) {
	if (evt.getKeyCode() == KeyEvent.VK_ENTER)
	  setVisible(false);		  
      }
    });

    pack();
    setVisible(true);
  }  
}
