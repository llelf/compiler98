package nhctracer;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

public class ServerDialog extends Dialog {
  Font font = new Font("Serif", Font.BOLD, 16);
  Button ok = new Button("OK");
  Button cancel = new Button("Cancel");
  int port;
  String server;

  TraceFrame f;

  TextField serverField;
  TextField portField;
  
  ServerDialog(TraceFrame _f, String _server, int _port) {
    super(_f, "Changer server/port", true);
    server = _server;
    port = _port;
    this.f = _f;
    setFont(font);
    setLayout(new GridLayout(3,0));

    Panel p1 = new Panel(new FlowLayout(FlowLayout.LEFT));
    p1.add(new Label("Server: "), Label.LEFT);
    serverField = new TextField(server, 40);
    p1.add(serverField);
    add(p1);

    Panel p2 = new Panel(new FlowLayout(FlowLayout.LEFT));
    p2.add(new Label("Port: "), Label.LEFT);
    portField = new TextField(""+port, 6);
    p2.add(portField);
    add(p2);

    Panel p3 = new Panel(new FlowLayout()); 
    p3.add(ok);
    p3.add(cancel);
    add(p3);

    addMouseListener(new MouseAdapter() {
      public void mouseEntered(MouseEvent evt) {
	serverField.requestFocus();
      }      
    });

    ActionListener okListener = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
	try {
	  port = Integer.parseInt(portField.getText());
	  server = serverField.getText();
	  if (server.equals("")) {
	    server = null;
	    new ModalMessage(f, "Server field cannot be empty.");	    
	  }
	} catch (NumberFormatException ex) {
	  port = 0;
	}
	if ((port <= 0) || (port > 65535)) {
	  port = 0;
	  new ModalMessage(f, "Bad port number.");
	}
	if ((server != null) && (port > 0))
	  setVisible(false);    
      }
    };

    ok.addActionListener(okListener);
    serverField.addActionListener(okListener);
    portField.addActionListener(okListener);
  
    cancel.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
	server = null;
	port = 0;
	setVisible(false);
      }
    });
    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
	server = null;
	port = 0;
	setVisible(false);	
      }
    });
    pack();
    serverField.requestFocus();
    setVisible(true);
  }  
}
