package nhctracer;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

public class FontDialog extends Dialog {
  Font font = new Font("Serif", Font.BOLD, 16);
  Button ok = new Button("OK");
  Button cancel = new Button("Cancel");
  int style, size;
  Font savedfont;  
  UI ui;

  TraceFrame f;

  Choice fontname = new Choice();
  Choice fontstyle = new Choice();
  TextField fontsize  = new TextField(4);
  
  Canvas display = new Canvas();
  
  public void init() {
    savedfont = ui.normalfont;
    String cfont = ui.normalfont.getName();
    // Get around bug in font name:
    for (int i = 0; i < fontname.getItemCount(); i++) {
      if (fontname.getItem(i).startsWith(cfont)) {
	fontname.select(i);
	break;
      }	
    }
    style = ui.normalfont.getStyle();
    size = ui.normalfont.getSize();
    
    if (style == Font.BOLD)
      fontstyle.select(1);
    if (style == Font.ITALIC)
      fontstyle.select(2);
    if (style == (Font.ITALIC | Font.BOLD))
      fontstyle.select(3);
    fontsize.setText(""+size);
  }

  FontDialog(TraceFrame _f, UI _ui) {
    super(_f, "Select font", true);
    this.f = _f;
    this.ui = _ui;
    setFont(font);
    setLayout(new BorderLayout());

    display = new Canvas() {
      public void paint(Graphics g) {
	g.setColor(Color.white);
	g.fillRect(0, 0, getSize().width, getSize().height);
	g.setColor(Color.black);
	g.setFont(new Font(fontname.getSelectedItem(), style, size));
	g.drawString("Haskell Tracer", 10, g.getFontMetrics().getHeight()+10);
	ui.normalfont = new Font(fontname.getSelectedItem(), style, size);
	ui.normalfm = getFontMetrics(ui.normalfont);
	f.mainPanel.dbgPanel.repaint();
      }
    };

    String[] fonts = Toolkit.getDefaultToolkit().getFontList();
    for (int i = 0; i < fonts.length; i++) {      
      fontname.add(fonts[i]); 
    }
    fontstyle.add("Plain");
    fontstyle.add("Bold");
    fontstyle.add("Italic");
    fontstyle.add("Bold Italic");

    init();

    Panel p1 = new Panel(new FlowLayout(FlowLayout.LEFT));
    p1.add(new Label("Font: "), Label.LEFT);
    p1.add(fontname);
    Panel p2 = new Panel(new FlowLayout(FlowLayout.LEFT));
    p2.add(new Label("Style: "), Label.LEFT);
    p2.add(fontstyle);
    Panel p0 = new Panel(new FlowLayout(FlowLayout.LEFT));
    p0.add(new Label("Size: "), Label.LEFT);
    p0.add(fontsize);

    
    Panel p3 = new Panel(new GridLayout(3,1));
    p3.add(p1);
    p3.add(p2);
    p3.add(p0);

    Panel p5 = new Panel(new FlowLayout()); 
    p5.add(ok);
    p5.add(cancel);

    add(p3, BorderLayout.NORTH);
    display.setSize(200, 40);
    add(display, BorderLayout.CENTER);
    add(p5, BorderLayout.SOUTH);

    fontname.addItemListener(new ItemListener() {
      public void itemStateChanged(ItemEvent ie) {
	display.repaint();
      }
    });

    fontstyle.addItemListener(new ItemListener() {
      public void itemStateChanged(ItemEvent ie) {
	String stylename = fontstyle.getSelectedItem();
	switch (fontstyle.getSelectedIndex()) {
	  case 0: style = Font.PLAIN; break;
	  case 1: style = Font.BOLD; break;
	  case 2: style = Font.ITALIC; break;
	  case 3: style = Font.BOLD | Font.ITALIC; break;
	}
	display.repaint();
      }
    });

    fontsize.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
	try {
	  size = Integer.parseInt(fontsize.getText());
	} catch (NumberFormatException e) {
	  size = 0;
	}
	fontsize.setText(""+size);
	display.repaint();
      }
    });

    ok.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
	ui.normalfont = new Font(fontname.getSelectedItem(), style, size);
	ui.normalfm = getFontMetrics(ui.normalfont);
	f.mainPanel.dbgPanel.repaint();
	setVisible(false);
      }
    });
    cancel.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
	ui.normalfont = savedfont;
	ui.normalfm = getFontMetrics(ui.normalfont);
	f.mainPanel.dbgPanel.repaint();
	setVisible(false);
      }
    });
    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
	setVisible(false);	
      }
    });
    pack();
    setVisible(true);
  }  

  public void setVisible(boolean visible) {
    init();
    super.setVisible(visible);
  }
}
