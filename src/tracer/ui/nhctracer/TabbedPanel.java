package nhctracer;

import java.awt.*;
import java.awt.event.*;
import java.util.Vector;

public class TabbedPanel extends Panel {
  TabSelector tab;		// component for choosing panel
  TabbedDisplayPanel disp;	// where other panels are displayed
  CardLayout card;

  TabbedPanel()	{
    Color hi = new Color(230,230,230), lo = new Color(50,50,50);
    setLayout(new BorderLayout());
    add("North",tab = new TabSelector(hi, lo));
    add("Center",disp = new TabbedDisplayPanel(hi, lo));
    disp.setLayout(card = new CardLayout());
  }

  // addItem
  // Add a component to be chosen by a tab with the given name
  void addItem(String n, Component c) {
    tab.addItem(n);
    disp.addItem(n, c);
  }

  // select
  // Display a component in the panel
  void select(String n) {
    tab.choose(n);
    disp.choose(n);
  }

  // chose
  // Called back by a TabSelector object when the user clicks on a tab
  void chose(String n) {
    disp.choose(n);
  }
}

class TabSelector extends Canvas
{
  Color hi, lo;
  Vector name = new Vector();
  int chosen = 0;
  Font font = new Font("timesRoman", Font.PLAIN, 14);
  Font chfont = new Font(font.getName(), Font.BOLD, 14);
  private static final int SKEW = 10;

  TabSelector(Color h, Color l) {
    hi = h; lo = l;
    addMouseListener(new MouseHandler());
  }

  void addItem(String n) {
    name.addElement(n);
    paint(getGraphics());
  }

  void choose(String n) {
    for(int i=0; i<name.size(); i++)
      if (((String)name.elementAt(i)).equals(n)) {
	chosen = i;
	paint(getGraphics());
      }
  }

  public void paint(Graphics g) {
    if (g == null || name.size() == 0)
      return;
    g.setColor(Color.lightGray);
    g.fillRect(0, 0, getSize().width, getSize().height);
    int tw = getSize().width / name.size();
    int th = getSize().height;
    for(int i=0; i<name.size(); i++) {
      int x = tw*i;
      if (i == chosen) {
	g.setColor(lo);
	g.drawLine(x+tw-3-SKEW, 1, x+tw-3, th-1);
	g.drawLine(x+tw-4-SKEW, 2, x+tw-4, th-1);
	g.setColor(hi);
	g.drawLine(x+SKEW, 0, x, th-1);
	g.drawLine(x+1+SKEW, 0, x+1, th-1);
	g.drawLine(x+SKEW, 0, x+tw-4-SKEW, 0);
	g.drawLine(x+SKEW, 1, x+tw-5-SKEW, 1);
	g.drawLine(x+tw-3, th-1, x+tw-1, th-1);
	g.drawLine(x+tw-3, th-2, x+tw-1, th-2);
      }
      else {
	g.setColor(lo);
	g.drawLine(x+tw-3-SKEW, 6, x+tw-3, th-1);
	g.drawLine(x+tw-4-SKEW, 7, x+tw-4, th-1);
	g.setColor(hi);
	g.drawLine(x+SKEW, 5, x, th-1);
	g.drawLine(x+1+SKEW, 5, x+1, th-1);
	g.drawLine(x+SKEW, 5, x+tw-4-SKEW, 5);
	g.drawLine(x+SKEW, 6, x+tw-5-SKEW, 6);
	g.drawLine(x, th-1, x+tw-1, th-1);
	g.drawLine(x, th-2, x+tw-1, th-2);
      }
      g.setColor(lo);
      if (i == chosen) 
	g.setFont(chfont);
      else 
	g.setFont(font);
      String str = (String)name.elementAt(i);
      int textw = g.getFontMetrics().stringWidth(str);
      int texth = g.getFontMetrics().getHeight();
      if (textw < tw-5)
	g.drawString(str, x+(tw-textw)/2, (th-texth)/2+texth);
    }
  }

  class MouseHandler extends MouseAdapter {
    public void mousePressed(MouseEvent evt) {
      int x = evt.getX();
      int y = evt.getY();
      if (name.size() > 0) {
	chosen = x / (getSize().width / name.size());
	paint(getGraphics());
	((TabbedPanel)getParent()).chose((String)name.elementAt(chosen));
      }
    }
  }

  public Dimension getMinimumSize() {
    return new Dimension(50,25);
  }

  public Dimension getPreferredSize() {
    return getMinimumSize();
  }
}

class TabbedDisplayPanel extends Panel
{
  Color hi, lo;
  CardLayout card;
  String chosen = null;

  TabbedDisplayPanel(Color h, Color l) {
    hi = h; lo = l;
    setLayout(card = new CardLayout());
  }

  // addItem
  // Add one component to the set of possibles to be shown
  void addItem(String n, Component c) {
    add(n, c);
    if (chosen == null)
      chosen = n;
  }

  // choose
  // Display the named panel
  void choose(String n)	{
    chosen = n;
    ((CardLayout)getLayout()).show(this, n);
    /*
    setBounds(getParent().getBounds());
    validate();
    update(getGraphics());
    */
    /*
    validate();
    show();
    hide();
    show();
    System.err.println("Chose: " + n);
    */
  }

  public Insets getInsets() {
    return new Insets(5,5,5,5);
  }

  public void paint(Graphics g)	{
    g.setColor(hi);
    g.drawLine(0, 0, 0, getSize().height-1);
    g.drawLine(1, 0, 1, getSize().height-1);
    g.setColor(lo);
    g.drawLine(0, getSize().height-1, getSize().width-1, getSize().height-1);
    g.drawLine(0, getSize().height-2, getSize().width-1, getSize().height-2);
    g.drawLine(getSize().width-1, getSize().height-1, getSize().width-1, 0);
    g.drawLine(getSize().width-2, getSize().height-1, getSize().width-2, 0);
  }
}

