package nhctracer;

import java.awt.*;
//import com.sun.java.swing.*;

public class Status extends TextField {
    Toolkit tk;
    String state = "";
    String text;

    Status(String text) {
      super(text);
      this.text = text;
      tk = Toolkit.getDefaultToolkit();
      setEditable(false);
      tk.sync();
    }

    public void setStateText(String state) {
      this.state = state;
      setText(text);
    }

    public void setText(String newText) {
	text = newText;
        super.setText(state + text);
	tk.sync();
    }

    public void normalCursor() {
      //tk.setCursor(Frame.DEFAULT_CURSOR);			
      tk.sync();
    }

    public void waitCursor() {
      //parent.setCursor(Frame.WAIT_CURSOR);
      tk.sync();
    }
}
